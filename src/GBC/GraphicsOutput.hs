{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.GraphicsOutput where

import           Control.Concurrent
import           Data.Foldable
import           Control.Monad.Reader
import           Data.Int
import           Data.StateVar
import           Data.Word
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
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
  , bgScanline :: !VertexArrayObject
  , bgCharacterBuffer :: !(Ptr Word8)
  , bgBackground :: !(Ptr Word8)
  , registers :: !(Ptr GLint)
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
    glState <- liftIO setUpOpenGL

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

          -- Update the registers
          when updateRegisters $ do
            for_ [0 .. 11] $ \register -> do
              byte <- readByte (0xFF40 + fromIntegral register)
              liftIO (pokeElemOff (registers glState) register (fromIntegral byte))
            glFlushMappedBufferRange GL_UNIFORM_BUFFER 0 48

          when updateVRAM $ do
            withVRAMPointer (\vram -> copyArray (bgCharacterBuffer glState) vram 0x1800)
            withBGPointer (\bg -> copyArray (bgBackground glState) bg 0x400)

          -- Signal that we're done updating VRAM.
          doneReadingVRAM
          bindVertexArrayObject (bgScanline glState)
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
setUpOpenGL :: IO GLState
setUpOpenGL = do
  glViewport 0 0 160 144

  gbVert    <- B.readFile "shaders/gb.vert"
  gbFrag    <- B.readFile "shaders/gb.frag"
  bgProgram <- compileShaders [(VertexShader, gbVert), (FragmentShader, gbFrag)]
  useProgram bgProgram

  bgProjection     <- linkUniform bgProgram "projection"
  projectionMatrix <- makeMatrix
    (join [[2.0 / 160, 0, 0, 0], [0, -2.0 / 144, 0, 0], [0, 0, 1, 0], [-1, 1, 0, 1]])
  bgProjection $= projectionMatrix

  bgScanline <- genVertexArrayObject
  bindVertexArrayObject bgScanline
  void $ makeVertexBuffer (join [[0, 0 :: Int32], [160, 0], [160, 1], [0, 1]])
  linkAttribute bgProgram position 0 8
  void $ makeElementBuffer (join [[0, 1, 2], [2, 3, 0]])

  texCharacterData             <- linkUniform bgProgram "texCharacterData"
  bgCharacterBuffer            <- setUpTextureBuffer (TextureUnit 0) texCharacterData 0x1800

  texBackgroundData            <- linkUniform bgProgram "texBackgroundData"
  bgBackground                 <- setUpTextureBuffer (TextureUnit 1) texBackgroundData 0x400

  (registersBuffer, registers) <- makeWritablePersistentBuffer ExplicitFlush UniformBuffer 48
  linkUniformBuffer bgProgram "Registers" registersBuffer 0

  pure GLState { .. }

 where
  setUpTextureBuffer textureUnit sampler size = do
    activeTextureUnit textureUnit
    sampler $= textureUnit
    texture <- genTexture
    bindTexture TextureBuffer texture
    (buffer, ptr) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer size
    linkTextureBuffer buffer
    pure ptr
