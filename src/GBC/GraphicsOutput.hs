{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.GraphicsOutput
  ( VideoBuffers(..)
  , startOutput
  )
where

import           Control.Concurrent
import           GBC.Memory
import           Control.Monad.Reader
import           Data.Int
import           Data.StateVar
import           Foreign.Ptr
import           GBC.Graphics
import           GLUtils
import           Graphics.GL.Core44
import qualified Data.ByteString               as B
import qualified SDL

data WindowContext = WindowContext {
    window :: !SDL.Window
  , queue :: !(MVar (Maybe Update))
  , glContext :: !SDL.GLContext
  , glState :: !GLState
}

-- | OpenGL state variables.
data GLState = GLState {
    bgProgram :: !Program
  , bgProjection :: !(StateVar GLmatrix4)
  , bgScanline :: !VertexArrayObject
}

startOutput :: IO (MVar (Maybe Update), VideoBuffers, SDL.Window)
startOutput = do
  let glConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 4 }
  window <- liftIO $ SDL.createWindow
    "Graphics output"
    SDL.defaultWindow { SDL.windowInitialSize     = SDL.V2 160 144
                      , SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
                      }

  queue        <- liftIO newEmptyMVar
  videoBuffers <- newEmptyMVar

  void $ liftIO $ forkOS $ do
    glContext <- SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
    (glState, buffers) <- liftIO setUpOpenGL
    putMVar videoBuffers buffers

    runReaderT eventLoop WindowContext { .. }
    SDL.destroyWindow window

  buffers <- takeMVar videoBuffers
  pure (queue, buffers, window)

eventLoop :: ReaderT WindowContext IO ()
eventLoop = do
  WindowContext {..} <- ask
  mupdate            <- liftIO (readMVar queue)
  let doneReadingVRAM = void (liftIO (takeMVar queue))

  case mupdate of
    Nothing                  -> doneReadingVRAM
    Just (Update updateMode) -> do
      case updateMode of
        VBlank -> do
          doneReadingVRAM
          liftIO (SDL.glSwapWindow window)
        ReadVRAM -> do
          fence <- insertFence
          bindVertexArrayObject (bgScanline glState)
          glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
          syncResult <- waitForFence fence 5000000000
          unless (syncResult == AlreadySignaled || syncResult == ConditionSatisfied)
            $ liftIO (print syncResult)
          doneReadingVRAM
        _ -> doneReadingVRAM
      eventLoop

-- | Position of the scanline.
position :: Attribute
position = Attribute "position" 2 Ints KeepInteger

-- | Configure OpenGL.
setUpOpenGL :: IO (GLState, VideoBuffers)
setUpOpenGL = do
  glViewport 0 0 160 144

  gbVert    <- B.readFile "shaders/bg.vert"
  gbFrag    <- B.readFile "shaders/bg.frag"
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

  (vramBuffer, vram) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer 0x2000
  setUpTextureBuffer bgProgram "texCharacterData"  (TextureUnit 0) vramBuffer 0      0x1800
  setUpTextureBuffer bgProgram "texBackgroundData" (TextureUnit 1) vramBuffer 0x1800 0x800

  (oamBuffer      , oam      ) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer 160
  (registersBuffer, registers) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer 48

  linkUniformBuffer bgProgram "Registers" registersBuffer 0

  pure (GLState { .. }, VideoBuffers { .. })

 where
  setUpTextureBuffer program uniform textureUnit buffer offset size = do
    activeTextureUnit textureUnit
    sampler <- linkUniform program uniform
    sampler $= textureUnit
    texture <- genTexture
    bindTexture TextureBuffer texture
    linkTextureBufferRange buffer offset size
