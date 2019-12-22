{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Graphics.PPU
  ( startOutput
  )
where

import           Common
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Bits
import           Data.Int
import           Data.StateVar
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Graphics
import           GBC.Graphics.VRAM
import           GBC.Memory
import           GBC.Mode
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
  , buffers :: !VRAM
  , memory :: !Memory
  , mode :: !EmulatorMode
  -- | This is a temporary area for blending the background, window, and
  -- sprites. Each byte represents one pixel.
  -- bit 7: Priority to background
  -- bit 6 ~ 5: DMG palette
  -- bit 4 ~ 2: CGB palette
  -- bit 1 ~ 0: Index into palette
  , lineAssemblySpace :: !(ForeignPtr Word8)
  -- | Each byte in this buffer corresponds to one of the pixels in the
  -- lineAssembly space. When a sprite pixel is drawn, the x-position of that
  -- sprite is written to the spritePriorityBuffer. This allows us to simulate
  -- the DMG behavior where sprites with lower x-positions take priority.
  , spritePriorityBuffer :: !(ForeignPtr Int8)
}

dmgBackgroundTileAttrs :: Word8
dmgBackgroundTileAttrs = 0

{-# INLINE getBlendInfo #-}
getBlendInfo :: Word8 -> Word8
getBlendInfo attrs =
  let priorityToBackground = isFlagSet flagDisplayPriority attrs
      cgbPalette           = attrs .&. 7
  in  (if priorityToBackground then 0x80 else 0) .|. (cgbPalette `unsafeShiftL` 2)

{-# INLINE getSpriteBlendInfo #-}
getSpriteBlendInfo :: Word8 -> Word8
getSpriteBlendInfo attrs =
  let priorityToBackground = isFlagSet flagDisplayPriority attrs
      dmgPalette           = isFlagSet flagOAMPalette attrs
      cgbPalette           = attrs .&. 7
  in  (if priorityToBackground then 0x80 else 0)
        .|. (cgbPalette `unsafeShiftL` 2)
        .|. (if dmgPalette then 0x40 else 0x20)

decodeBlendInfo :: Word8 -> (Word8, Int)
decodeBlendInfo blendInfo =
  let palette = (blendInfo `unsafeShiftR` 5) .&. 3
      index   = blendInfo .&. 3
  in  (palette, fromIntegral index)

cgbDecodeBlendInfo :: Word8 -> (Word8, Bool)
cgbDecodeBlendInfo blendInfo = (blendInfo .&. 0x1F, blendInfo .&. blendInfoDMGPaletteMask /= 0)

blendInfoDMGPaletteMask :: Word8
blendInfoDMGPaletteMask = 0x60

blendInfoIndexMask :: Word8
blendInfoIndexMask = 3

-- | OpenGL state variables.
data GLState = GLState {
    scaleProgram :: !Program
  , frameVAO :: !VertexArrayObject
  , frameTexture :: !Texture
  , frameTextureBuffer :: !BufferObject
  , frameTextureBufferBytes :: !(Ptr Word8)
}

-- | Initialize a window, and start the rendering thread.
startOutput :: Memory -> VRAM -> GraphicsSync -> EmulatorMode -> IO SDL.Window
startOutput memory buffers sync mode = do
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

  let outputBase = frameTextureBufferBytes glState `plusPtr` (fromIntegral line * 640)
  withForeignPtr lineAssemblySpace
    $ \lineAssembly -> withForeignPtr spritePriorityBuffer $ \priorityBuffer ->
        runReaderT (renderLine mode line buffers lineAssembly priorityBuffer outputBase) memory

  -- Relinquish exclusive access to VRAM
  putMVar (hblankStart sync) ()

  when (line == 143) $ do
    bindBuffer PixelUpload (frameTextureBuffer glState)
    glFlushMappedBufferRange GL_PIXEL_UNPACK_BUFFER 0 (160 * 144 * 4)
    bindTexture Texture2D (frameTexture glState)
    glTexSubImage2D GL_TEXTURE_2D 0 0 0 160 144 GL_RGBA GL_UNSIGNED_BYTE nullPtr
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
    glFinish
    SDL.glSwapWindow window

  unless (line == 255) $ eventLoop context

renderLine
  :: EmulatorMode -> Word8 -> VRAM -> Ptr Word8 -> Ptr Int8 -> Ptr Word8 -> ReaderT Memory IO ()
renderLine mode line vram assemblySpace priorityBuffer outputBase = do
  scx  <- readByte SCX
  scy  <- readByte SCY
  wx   <- readByte WX
  wy   <- readByte WY
  lcdc <- readByte LCDC

  let bgEnabled      = isFlagSet flagBackgroundEnable lcdc
  let windowEnabled  = isFlagSet flagWindowEnable lcdc && line >= wy && wx <= 166
  let spritesEnabled = isFlagSet flagOBJEnable lcdc
  let fontOffset     = if isFlagSet flagTileDataSelect lcdc then 0 else 0x1000
  let windowStart    = if windowEnabled then fromIntegral wx - 7 else 160

  -- Render window data to the assembly area.
  when bgEnabled $ do
    let yTile      = fromIntegral $ (line + scy) `unsafeShiftR` 3
    let yOffset    = fromIntegral $ (line + scy) .&. 0x07
    let xTile      = fromIntegral $ scx `unsafeShiftR` 3
    let xOffset    = fromIntegral $ scx .&. 0x07

    let bgTiles = if isFlagSet flagBackgroundTileMap lcdc then 0x1C00 else 0x1800
    let bgTileLine = bgTiles + (32 * yTile)
    liftIO $ tileMachine bgTileLine fontOffset yOffset windowStart xTile (negate xOffset)

  -- Render window data to the assembly area.
  when windowEnabled $ do
    let yTileWindow    = fromIntegral $ (line - wy) `unsafeShiftR` 3
    let yOffsetWindow  = fromIntegral $ (line - wy) .&. 0x07

    let windowTiles    = if isFlagSet flagWindowTileMap lcdc then 0x1C00 else 0x1800
    let windowTileLine = windowTiles + (32 * yTileWindow)
    liftIO $ tileMachine windowTileLine fontOffset yOffsetWindow 160 0 windowStart

  -- Render sprite data to the assembly area.
  let spriteHeight = if isFlagSet flagOBJSize lcdc then 16 else 8
  when spritesEnabled $ liftIO $ doSprites spriteHeight

  -- Copy the data from the assembly area into the frame texture buffer,
  -- applying the GB palettes.
  if mode == CGB then applyCGBPalettes else applyDMGPalettes

 where
  -- Write tiles to the assembly area. Read from the line of tiles starting at
  -- tileBase, get the pixel data from fontOffset, output pixels from tile row
  -- yOffset, and stop rendering when the output offset is stopOffset.
  -- 
  -- Each round takes the offset to the tile to render, and the x position to
  -- render the pixels to.
  tileMachine !tileBase !fontOffset !yOffset !stopOffset = go
   where
    go !inPos !outPos = if outPos >= stopOffset
      then pure ()
      else do
        tile      <- readTile vram tileBase inPos
        tileAttrs <- if mode == DMG
          then pure dmgBackgroundTileAttrs
          else readTileAttrs vram tileBase inPos
        let hflip        = isFlagSet flagHorizontalFlip tileAttrs
        let vflip        = isFlagSet flagVerticalFlip tileAttrs
        let tileDataBase = if tile > 127 then 0 else fontOffset
        let fontLineOffset = if vflip
              then (fromIntegral tile * 16) + ((7 - yOffset) * 2)
              else (fromIntegral tile * 16) + (yOffset * 2)
        (byte0, byte1) <- if isFlagSet flagBank tileAttrs
          then readBankedTileData vram (tileDataBase + fontLineOffset)
          else readTileData vram (tileDataBase + fontLineOffset)
        outPos' <- pixelMachine byte0 byte1 (getBlendInfo tileAttrs) hflip 7 outPos
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
  pixelMachine !byte0 !byte1 !blendInfo !hflip = go
   where
    go !byteOffset !pixelOffset = if pixelOffset < 0
      then go (byteOffset - 1) (pixelOffset + 1)
      else do
        let pixel = decodePixel byte0 byte1 (if hflip then 7 - byteOffset else byteOffset)
        pokeElemOff assemblySpace pixelOffset (blendInfo .|. pixel)
        if byteOffset == 0 then pure (pixelOffset + 1) else go (byteOffset - 1) (pixelOffset + 1)

  -- Write sprites to the assembly area.
  doSprites h = go 0 0
   where
    go !offset !spritesRendered = if offset >= 40 || spritesRendered >= (10 :: Int)
      then pure ()
      else do
        spriteRendered <- doSprite h (offset * 4)
        go (offset + 1) (if spriteRendered then spritesRendered + 1 else spritesRendered)

  doSprite !h !offset = do
    (y, x) <- readSpritePosition vram offset
    let spriteVisible = x /= 0 && line + 16 >= y && line + 16 < y + h
    if not spriteVisible
      then pure False
      else do
        (rawCode, attrs) <- readSpriteAttributes vram offset
        let code  = if h == 16 then rawCode .&. 0xFE else rawCode
        let vflip = isFlagSet flagVerticalFlip attrs
        let hflip = isFlagSet flagHorizontalFlip attrs
        let fontLineOffset = (16 * fromIntegral code) + 2 * fromIntegral
              (if vflip then y + h - 17 - line else line + 16 - y)
        let blendInfo = getSpriteBlendInfo attrs
        let xOffset   = fromIntegral x - 8
        (byte0, byte1) <- if mode == CGB && isFlagSet flagBank attrs
          then readBankedTileData vram fontLineOffset
          else readTileData vram fontLineOffset

        let priority = if mode == CGB then 0 else fromIntegral xOffset
        blendSprite priority hflip blendInfo byte0 byte1 xOffset
        pure True

  blendSprite !priority !hflip !blendInfo !byte0 !byte1 !outOffset = go 0
   where
    go 8  = pure ()
    go !i = do
      let pixel = decodePixel byte0 byte1 (if hflip then i else 7 - i)
      unless (pixel == 0) $ do
        let outPos = outOffset + i
        previous         <- peekElemOff assemblySpace outPos
        previousPriority <- peekElemOff priorityBuffer outPos
        let drawOverSprite = previous .&. blendInfoDMGPaletteMask /= 0
        let bgPriority     = (blendInfo .|. previous) .&. 0x80 /= 0
        when
            (  (drawOverSprite && priority < previousPriority)
            || (  not drawOverSprite
               && (not bgPriority || (bgPriority && (previous .&. blendInfoIndexMask == 0)))
               )
            )
          $ do
              pokeElemOff assemblySpace  outPos (blendInfo .|. pixel)
              pokeElemOff priorityBuffer outPos priority
      go (i + 1)

  -- Generate actual pixel data in the frame texture buffer based on the data in
  -- the assembly area.
  applyDMGPalettes = do
    bgp  <- readByte BGP
    obp0 <- readByte OBP0
    obp1 <- readByte OBP1

    let go !offset = do
          blendResult <- peekElemOff assemblySpace offset
          let (palette, index) = decodeBlendInfo blendResult
          let paletteData = case palette of
                0 -> bgp :: Word8
                1 -> obp0
                2 -> obp1
                _ -> error "Framebuffer corrupted"
          let pixel = (3 - ((paletteData `unsafeShiftR` (fromIntegral index * 2)) .&. 0x03)) * 85
          let outputOffset = offset * 4
          pokeElemOff outputBase outputOffset       pixel
          pokeElemOff outputBase (outputOffset + 1) pixel
          pokeElemOff outputBase (outputOffset + 2) pixel
          if offset >= 159 then pure () else go (offset + 1)

    liftIO (go 0)

  applyCGBPalettes = liftIO (go 0)
   where
    go !offset = do
      blendResult <- peekElemOff assemblySpace offset
      let (colorIndex, isForeground) = cgbDecodeBlendInfo blendResult
      color <- readRGBPalette vram isForeground colorIndex
      pokeElemOff (castPtr outputBase) offset color
      if offset >= 159 then pure () else go (offset + 1)

flagBank :: Word8
flagBank = 0x08

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
  glTexStorage2D GL_TEXTURE_2D 1 GL_RGBA8 160 144
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)

  (frameTextureBuffer, frameTextureBufferBytes) <- makeWritablePersistentBuffer ExplicitFlush
                                                                                PixelUpload
                                                                                (160 * 144 * 4)

  pure GLState { .. }