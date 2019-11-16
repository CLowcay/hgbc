{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Graphics
  ( VideoBuffers(..)
  , initBuffers
  , startOutput
  )
where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Bits
import           Data.Int
import           Data.StateVar
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.GraphicsSync
import           GBC.Memory
import           GBC.Registers
import           GLUtils
import           Graphics.GL.Core44
import qualified Data.ByteString               as B
import qualified SDL

-- | Window state.
data WindowContext = WindowContext {
    window :: !SDL.Window
  , sync :: !GraphicsSync
  , glState :: !GLState
  , buffers :: !VideoBuffers
  , memory :: !Memory
  -- | This is a temporary area for blending the background, window, and
  -- sprites. Each byte represents one pixel. The low 2 bits contain the index
  -- into the palette. The next 3 bits contain the palette number. We can
  -- distinguish between pixels that contain background data vs sprite data by
  -- checking the palette number.  This helps us to implement sprite priority.
  , lineAssemblySpace :: !(ForeignPtr Word8)
  -- | Each byte in this buffer corresponds to one of the pixels in the
  -- lineAssembly space. When a sprite pixel is drawn, the x-position of that
  -- sprite is written to the spritePriorityBuffer. This allows us to simulate
  -- the DMG behavior where sprites with lower x-positions take priority.
  , spritePriorityBuffer :: !(ForeignPtr Int8)
}

-- | OpenGL state variables.
data GLState = GLState {
    scaleProgram :: !Program
  , frameVAO :: !VertexArrayObject
  , frameTexture :: !Texture
  , frameTextureBuffer :: !BufferObject
  , frameTextureBufferBytes :: !(Ptr Word8)
}

-- | Initialize the video buffers.
initBuffers :: IO VideoBuffers
initBuffers = do
  vram <- mallocBytes 0x2000
  oam  <- mallocBytes 160
  pure VideoBuffers { .. }

-- | Initialize a window, and start the rendering thread.
startOutput :: Memory -> VideoBuffers -> GraphicsSync -> IO SDL.Window
startOutput memory buffers sync = do
  let glConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 4 }
  window <- SDL.createWindow
    "Graphics output"
    SDL.defaultWindow { SDL.windowInitialSize     = SDL.V2 320 288
                      , SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
                      }

  void $ forkOS $ do
    void (SDL.glCreateContext window)
    SDL.swapInterval $= SDL.SynchronizedUpdates
    lineAssemblySpace    <- mallocForeignPtrBytes 160
    spritePriorityBuffer <- mallocForeignPtrBytes 160
    glState              <- setUpOpenGL

    eventLoop WindowContext { .. }
    SDL.destroyWindow window

  pure window

-- | The main event loop for the renderer.
eventLoop :: WindowContext -> IO ()
eventLoop context@WindowContext {..} = do

  -- Acquire exclusive access to VRAM
  line <- takeMVar (currentLine sync)

  let outputBase = frameTextureBufferBytes glState `plusPtr` (fromIntegral line * 480)
  withForeignPtr lineAssemblySpace
    $ \lineAssembly -> withForeignPtr spritePriorityBuffer $ \priorityBuffer ->
        runReaderT (renderLine line buffers lineAssembly priorityBuffer outputBase) memory

  -- Relinquish exclusive access to VRAM
  putMVar (hblankStart sync) ()

  when (line == 143) $ do
    bindBuffer PixelUpload (frameTextureBuffer glState)
    glFlushMappedBufferRange GL_PIXEL_UNPACK_BUFFER 0 (160 * 144 * 3)
    bindTexture Texture2D (frameTexture glState)
    glTexSubImage2D GL_TEXTURE_2D 0 0 0 160 144 GL_RGB GL_UNSIGNED_BYTE nullPtr
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
    glFinish
    SDL.glSwapWindow window

  unless (line == 255) $ eventLoop context

renderLine :: Word8 -> VideoBuffers -> Ptr Word8 -> Ptr Int8 -> Ptr Word8 -> ReaderT Memory IO ()
renderLine line VideoBuffers {..} assemblySpace priorityBuffer outputBase = do
  scx  <- readByte SCX
  scy  <- readByte SCY
  wx   <- readByte WX
  wy   <- readByte WY
  lcdc <- readByte LCDC

  let bgEnabled      = lcdc .&. flagBackgroundEnable /= 0
  let windowEnabled  = lcdc .&. flagWindowEnable /= 0 && line >= wy && wx <= 166
  let spritesEnabled = lcdc .&. flagOBJEnable /= 0
  let fontOffset     = if lcdc .&. flagTileDataSelect == 0 then 0x1000 else 0

  let windowStart    = if windowEnabled then fromIntegral wx - 7 else 160

  -- Render window data to the assembly area.
  when bgEnabled $ do
    let yTile      = fromIntegral $ (line + scy) `unsafeShiftR` 3
    let yOffset    = fromIntegral $ (line + scy) .&. 0x07
    let xTile      = fromIntegral $ scx `unsafeShiftR` 3
    let xOffset    = fromIntegral $ scx .&. 0x07

    let bgTiles = vram `plusPtr` if lcdc .&. flagBackgroundTileMap == 0 then 0x1800 else 0x1C00
    let bgTileLine = bgTiles `plusPtr` (32 * yTile)
    liftIO $ tileMachine bgTileLine fontOffset yOffset windowStart xTile (negate xOffset)

  -- Render window data to the assembly area.
  when windowEnabled $ do
    let yTileWindow    = fromIntegral $ (line - wy) `unsafeShiftR` 3
    let yOffsetWindow  = fromIntegral $ (line - wy) .&. 0x07

    let windowTiles = vram `plusPtr` if lcdc .&. flagWindowTileMap == 0 then 0x1800 else 0x1C00
    let windowTileLine = windowTiles `plusPtr` (32 * yTileWindow)
    liftIO $ tileMachine windowTileLine fontOffset yOffsetWindow 160 0 windowStart

  -- Render sprite data to the assembly area.
  let spriteHeight = if lcdc .&. flagOBJSize == 0 then 8 else 16
  when spritesEnabled $ liftIO $ doSprites spriteHeight

  -- Copy the data from the assembly area into the frame texture buffer,
  -- applying the GB palettes.
  applyDMGPalettes

 where
  -- Write tiles to the assembly area. Read from the line of tiles starting at
  -- tileBase, get the pixel data from fontOffset, output pixels from tile row
  -- yOffset, and stop rendering when the output offset is stopOffset.
  -- 
  -- Each round takes the offset to the tile to render, and the x position to
  -- render the pixels to.
  tileMachine tileBase fontOffset yOffset stopOffset = go
   where
    go !inPos !outPos = if outPos >= stopOffset
      then pure ()
      else do
        tile <- peekElemOff tileBase inPos :: IO Word8
        let tileDataBase   = if tile > 127 then vram else vram `plusPtr` fontOffset
        let fontLineOffset = (fromIntegral tile * 16) + (yOffset * 2)
        byte0   <- peekElemOff tileDataBase fontLineOffset
        byte1   <- peekElemOff tileDataBase (fontLineOffset + 1)

        outPos' <- pixelMachine byte0 byte1 7 outPos
        go ((inPos + 1) .&. 0x1F) outPos'  -- wrap inPos back to 0 when it gets to the end of the line

  {-# INLINE decodePixel #-}
  decodePixel byte0 byte1 offset =
    0x03
      .&. (   ((byte0 `unsafeShiftR` offset) .&. 0x01)
          .|. ((byte1 `unsafeShiftR` offset) `unsafeShiftL` 1)
          )

  -- Write pixels to the assembly area. Takes two bytes containing the pixel
  -- data, the bit offset into the bytes, and the x position to output the
  -- pixels to. Returns the final x position. Negative values for outPos are
  -- acceptable, pixelMachine will skip pixels that were scheduled to go to
  -- negative positions.
  pixelMachine byte0 byte1 = go
   where
    go !byteOffset !pixelOffset = if pixelOffset < 0
      then go (byteOffset - 1) (pixelOffset + 1)
      else do
        pokeElemOff assemblySpace pixelOffset (decodePixel byte0 byte1 byteOffset)
        if byteOffset == 0 then pure (pixelOffset + 1) else go (byteOffset - 1) (pixelOffset + 1)

  -- Write sprites to the assembly area.
  doSprites h = go 0 0
   where
    go !offset !spritesRendered = if offset >= 40 || spritesRendered >= (10 :: Int)
      then pure ()
      else do
        spriteRendered <- doSprite h (offset * 4)
        go (offset + 1) (if spriteRendered then spritesRendered + 1 else spritesRendered)

  spriteY = peekElemOff oam
  spriteX offset = peekElemOff oam (offset + 1)
  spriteCode offset = peekElemOff oam (offset + 2)
  spriteAttrs offset = peekElemOff oam (offset + 3)

  doSprite h offset = do
    x <- spriteX offset
    y <- spriteY offset
    let spriteVisible = x /= 0 && line + 16 >= y && line + 16 < y + h
    if not spriteVisible
      then pure False
      else do
        code  <- spriteCode offset
        attrs <- spriteAttrs offset
        let vflip = attrs .&. flagVerticalFlip /= 0
        let hflip = attrs .&. flagHorizontalFlip /= 0
        let fontLineOffset = (16 * fromIntegral code) + 2 * fromIntegral
              (if vflip then y + h - 17 - line else line + 16 - y)
        let bgPriority = attrs .&. flagDisplayPriority /= 0
        let palette = if attrs .&. flagOAMPalette == 0 then 0x04 else 0x08
        let xOffset    = fromIntegral x - 8
        byte0 <- peekElemOff vram fontLineOffset
        byte1 <- peekElemOff vram (fontLineOffset + 1)

        if hflip
          then fastFor 0 8 $ \i -> blendSprite bgPriority
                                               (fromIntegral xOffset)
                                               (palette .|. decodePixel byte0 byte1 i)
                                               (xOffset + i)
          else fastFor 0 8 $ \i -> blendSprite bgPriority
                                               (fromIntegral xOffset)
                                               (palette .|. decodePixel byte0 byte1 (7 - i))
                                               (xOffset + i)
        pure True

  fastFor :: Int -> Int -> (Int -> IO ()) -> IO ()
  fastFor i0 i1 action = go i0
   where
    go !i = if i == i1
      then pure ()
      else do
        action i
        go (i + 1)

  {-# INLINE blendSprite #-}
  blendSprite bgPriority priority pixel outOffset = do
    previous         <- peekElemOff assemblySpace outOffset
    previousPriority <- peekElemOff priorityBuffer outOffset
    let drawOverSprite = previous .&. 0x0C /= 0
    when
        (  (drawOverSprite && priority < previousPriority)
        || (not drawOverSprite && (not bgPriority || (bgPriority && (previous == 0))))
        )
      $ when (pixel .&. 0x03 /= 0)
      $ do
          pokeElemOff assemblySpace  outOffset pixel
          pokeElemOff priorityBuffer outOffset priority

  -- Generate actual pixel data in the frame texture buffer based on the data in
  -- the assembly area.
  applyDMGPalettes = do
    bgp  <- readByte BGP
    obp0 <- readByte OBP0
    obp1 <- readByte OBP1

    let
      go !offset = do
        index <- peekElemOff assemblySpace offset
        let palette = case index `unsafeShiftR` 2 of
              0 -> bgp :: Word8
              1 -> obp0
              2 -> obp1
              _ -> error "Framebuffer corrupted"
        let pixel =
              (3 - ((palette `unsafeShiftR` (fromIntegral (index .&. 0x3) * 2)) .&. 0x03)) * 85
        let outputOffset = offset * 3
        pokeElemOff outputBase outputOffset       pixel
        pokeElemOff outputBase (outputOffset + 1) pixel
        pokeElemOff outputBase (outputOffset + 2) pixel
        if offset >= 159 then pure () else go (offset + 1)

    liftIO (go 0)

flagOAMPalette :: Word8
flagOAMPalette = 0x10

flagHorizontalFlip :: Word8
flagHorizontalFlip = 0x20

flagVerticalFlip :: Word8
flagVerticalFlip = 0x40

flagDisplayPriority :: Word8
flagDisplayPriority = 0x80

-- | Position of the scanline.
position :: Attribute
position = Attribute "position" 2 Ints PerVertex KeepInteger

-- | Configure OpenGL.
setUpOpenGL :: IO GLState
setUpOpenGL = do
  glViewport 0 0 320 288
  scaleVert    <- B.readFile "shaders/scale.vert"
  scaleFrag    <- B.readFile "shaders/scale.frag"
  scaleProgram <- compileShaders [(VertexShader, scaleVert), (FragmentShader, scaleFrag)]
  useProgram scaleProgram

  frameVAO <- genVertexArrayObject
  bindVertexArrayObject frameVAO
  void (makeVertexBuffer (join [[-1.0, -1.0 :: Float], [1.0, -1.0], [1.0, 1.0], [-1.0, 1.0]]))
  linkAttribute scaleProgram position 0 8
  void (makeElementBuffer (join [[0, 1, 2], [2, 3, 0]]))

  activeTextureUnit (TextureUnit 0)
  frameSampler <- linkUniform scaleProgram "frame"
  frameSampler $= TextureUnit 0
  frameTexture <- genTexture
  bindTexture Texture2D frameTexture
  glTexStorage2D GL_TEXTURE_2D 1 GL_RGB8 160 144
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)

  (frameTextureBuffer, frameTextureBufferBytes) <- makeWritablePersistentBuffer ExplicitFlush
                                                                                PixelUpload
                                                                                (160 * 144 * 3)

  pure GLState { .. }
