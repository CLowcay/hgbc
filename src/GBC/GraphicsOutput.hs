{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.GraphicsOutput where

import           Common
import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Int
import           Data.StateVar
import           Data.Word
import           Foreign.Ptr
import           GBC.Graphics
import           GBC.Memory
import           GLUtils
import           Graphics.GL.Core44
import qualified Data.ByteString               as B
import qualified SDL

data WindowContext = WindowContext {
    window :: !SDL.Window
  , memory :: !Memory
  , queue :: !(MVar (Maybe Update))
  , glContext :: !SDL.GLContext
  , glState :: !GLState
}

instance HasMemory WindowContext where
  forMemory = memory

-- | OpenGL state variables.
data GLState = GLState {
    bgProgram :: !Program
  , bgProjection :: !(StateVar GLmatrix4)
  , bgLine :: !(StateVar GLint)
  , bgVAO :: !VertexArrayObject
  , bgCharacterBuffer :: !BufferObject
  , bgBackground :: !BufferObject
  , bgSCX :: !(StateVar GLint)
  , bgSCY :: !(StateVar GLint)
  , bgBGP :: !(StateVar GLint)
  , bgBackgroundDataOffset :: !(StateVar GLint)
  , bgCharacterDataOffset :: !(StateVar GLint)
}

startOutput :: UsesMemory env m => ReaderT env m (MVar (Maybe Update), SDL.Window)
startOutput = do
  let glConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 4 }
  window <- liftIO $ SDL.createWindow
    "Graphics output"
    SDL.defaultWindow { SDL.windowInitialSize     = SDL.V2 160 144
                      , SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
                      }

  queue  <- liftIO newEmptyMVar
  memory <- asks forMemory

  void $ liftIO $ forkOS $ do
    glContext <- SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
    glState <- liftIO (setUpOpenGL memory)

    fence0  <- insertFence
    runReaderT (eventLoop fence0) WindowContext { .. }
    SDL.destroyWindow window
  pure (queue, window)

{-# SPECIALIZE readByte :: Word16 -> ReaderT WindowContext IO Word8 #-}
eventLoop :: Fence -> ReaderT WindowContext IO ()
eventLoop fence = do
  WindowContext {..} <- ask
  mupdate            <- liftIO $ readMVar queue
  let doneReadingVRAM = void . liftIO $ takeMVar queue

  case mupdate of
    Nothing          -> doneReadingVRAM
    Just Update {..} -> do
      fence' <- case updateMode of
        VBlank -> do
          doneReadingVRAM
          liftIO $ do
            SDL.glSwapWindow window
            pure fence
        ReadVRAM -> do
          -- Wait for previous commands to finish
          liftIO $ do
            syncResult <- waitForFence fence 5000000000
            unless (syncResult == AlreadySignaled || syncResult == ConditionSatisfied)
              $ print syncResult

          bgLine glState $= fromIntegral updateLine

          when updateRegisters $ do
            lcdc <- readByte LCDC
            bgCharacterDataOffset glState $= if isFlagSet flagTileDataSelect lcdc then 0 else 0x800
            bgBackgroundDataOffset glState
              $= if isFlagSet flagBackgroundTileMap lcdc then 0x400 else 0

            scxValue <- readByte SCX
            scyValue <- readByte SCY
            bgSCX glState $= fromIntegral scxValue
            bgSCY glState $= fromIntegral scyValue

            bgpValue <- readByte BGP
            bgBGP glState $= fromIntegral bgpValue

          when updateVRAM $ do
            bindBuffer TextureBufferBuffer (bgCharacterBuffer glState)
            withVRAMPointer (glBufferSubData GL_TEXTURE_BUFFER 0 0x1800)
            bindBuffer TextureBufferBuffer (bgBackground glState)
            withBGPointer (glBufferSubData GL_TEXTURE_BUFFER 0 0x400)

          -- Signal that we're done updating VRAM.
          doneReadingVRAM
          bindVertexArrayObject (bgVAO glState)
          glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
          insertFence
        _ -> do
          doneReadingVRAM
          pure fence
      eventLoop fence'

-- | Position of the scanline.
position :: Attribute
position = Attribute "position" 2 Ints KeepInteger

-- | Configure OpenGL.
setUpOpenGL :: Memory -> IO GLState
setUpOpenGL mem = do
  gbVert    <- B.readFile "shaders/gb.vert"
  gbFrag    <- B.readFile "shaders/gb.frag"
  bgProgram <- compileShaders [(VertexShader, gbVert), (FragmentShader, gbFrag)]
  useProgram bgProgram

  bgProjection           <- linkUniform bgProgram "projection"
  bgLine                 <- linkUniform bgProgram "line"
  bgSCX                  <- linkUniform bgProgram "scx"
  bgSCY                  <- linkUniform bgProgram "scy"
  bgBGP                  <- linkUniform bgProgram "bgp"
  bgBackgroundDataOffset <- linkUniform bgProgram "backgroundDataOffset"
  bgCharacterDataOffset  <- linkUniform bgProgram "characterDataOffset"

  bgVAO                  <- genVertexArrayObject
  bindVertexArrayObject bgVAO
  void $ makeVertexBuffer (join [[0, 0 :: Int32], [160, 0], [160, 1], [0, 1]])
  linkAttribute bgProgram position 0 8
  void $ makeElementBuffer (join [[0, 1, 2], [2, 3, 0]])

  samplerCharacterData <- linkUniform bgProgram "texCharacterData"
  bgCharacterBuffer    <- setUpTextureBuffer (TextureUnit 0)
                                             samplerCharacterData
                                             withVRAMPointer
                                             0x1800

  samplerBackgroundData <- linkUniform bgProgram "texBackgroundData"
  bgBackground <- setUpTextureBuffer (TextureUnit 1) samplerBackgroundData withBGPointer 0x400

  glViewport 0 0 160 144

  projectionMatrix <- makeMatrix
    (join [[2.0 / 160, 0, 0, 0], [0, -2.0 / 144, 0, 0], [0, 0, 1, 0], [-1, 1, 0, 1]])

  bgProjection $= projectionMatrix

  pure GLState { .. }

 where
  setUpTextureBuffer
    :: TextureUnit
    -> StateVar TextureUnit
    -> ((Ptr a -> IO ()) -> ReaderT Memory IO ())
    -> GLsizeiptr
    -> IO BufferObject
  setUpTextureBuffer textureUnit sampler withData size = do
    activeTextureUnit textureUnit
    sampler $= textureUnit
    texture <- genTexture
    bindTexture TextureBuffer texture
    textureBuffer <- genBuffer
    bindBuffer TextureBufferBuffer textureBuffer
    flip runReaderT mem $ withData $ \ptr -> glBufferData GL_TEXTURE_BUFFER size ptr GL_STREAM_DRAW
    linkTextureBuffer textureBuffer
    pure textureBuffer
