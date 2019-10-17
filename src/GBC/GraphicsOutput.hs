{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.GraphicsOutput where

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           GBC.Graphics
import           GBC.Memory
import           GLUtils
import           SDL                            ( ($=) )
import qualified Data.ByteString               as B
import qualified Graphics.Rendering.OpenGL     as GL
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
    bgProgram :: !GL.Program
  , bgProjection :: !(GL.StateVar (GL.GLmatrix Float))
  , bgLine :: !(GL.StateVar GL.GLint)
  , bgVAO :: !GL.VertexArrayObject
}

startOutput :: UsesMemory env m => ReaderT env m (MVar (Maybe Update), SDL.Window)
startOutput = do
  let glConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 3 2 }
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

    fence0  <- liftIO GL.syncGpuCommandsComplete
    runReaderT (eventLoop fence0) WindowContext { .. }
    SDL.destroyWindow window
  pure (queue, window)

{-# SPECIALIZE readByte :: Word16 -> ReaderT WindowContext IO Word8 #-}

eventLoop :: GL.SyncObject -> ReaderT WindowContext IO ()
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
            GL.clearColor $= GL.Color4 1 1 1 1
            GL.clear [GL.ColorBuffer]
            pure fence
        ReadVRAM -> do
          -- Wait for previous commands to finish
          liftIO $ do
            syncResult <- GL.clientWaitSync fence [] 5000000
            unless (syncResult == GL.AlreadySignaled || syncResult == GL.ConditionSatisfied)
              $ print syncResult

          bgLine glState $= fromIntegral updateLine

          -- Signal that we're done updating VRAM.
          doneReadingVRAM
          GL.bindVertexArrayObject $= Just (bgVAO glState)
          liftIO $ do
            GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr
            GL.syncGpuCommandsComplete
        _ -> do
          doneReadingVRAM
          pure fence
      eventLoop fence'

-- | The projection matrix uniform.
projection :: Uniform (GL.GLmatrix Float)
projection = Uniform "projection"

-- | The LCD line number.
lineNumber :: Uniform GL.GLint
lineNumber = Uniform "line"

-- | Position of the scanline.
position :: Attribute (GL.Vector2 GL.GLint)
position = Attribute "position" 2 GL.Int GL.KeepIntegral

-- | Configure OpenGL.
setUpOpenGL :: IO GLState
setUpOpenGL = do
  bgProgram    <- loadShaders
  bgProjection <- linkUniform bgProgram projection
  bgLine       <- linkUniform bgProgram lineNumber

  bgVAO        <- newVertexArrayObject
  void $ loadVertexData (join [[0, 0 :: Int32], [160, 0], [160, 1], [0, 1]])
  linkAttribute bgProgram position 0 8
  void $ loadElementData (join [[0, 1, 2], [2, 3, 0]])

  GL.viewport $= (GL.Position 0 0, GL.Size 160 144)

  projectionMatrix <- GL.newMatrix
    GL.ColumnMajor
    (join [[2.0 / 160, 0, 0, 0], [0, -2.0 / 144, 0, 0], [0, 0, 1, 0], [-1, 1, 0, 1]])

  bgProjection $= projectionMatrix

  pure GLState { .. }

loadShaders :: IO GL.Program
loadShaders = do
  program <- GL.createProgram
  loadShader program GL.VertexShader =<< B.readFile "shaders/gb.vert"
  loadShader program GL.FragmentShader =<< B.readFile "shaders/gb.frag"

  GL.bindFragDataLocation program "outColor" $= 0
  GL.linkProgram program
  GL.currentProgram $= Just program

  pure program
