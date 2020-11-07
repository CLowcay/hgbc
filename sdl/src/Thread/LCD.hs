{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Thread.LCD
  ( start,
  )
where

import Control.Concurrent
  ( forkOS,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
  )
import Control.Exception (mask, try)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import qualified Data.ByteString as B
import Data.FileEmbed (embedOneFileOf)
import Data.Functor ((<&>))
import Data.StateVar (HasSetter (($=)), StateVar)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.Ptr (Ptr, nullPtr)
import qualified GLUtils as GL
import qualified Graphics.GL.Core44 as GL
import qualified HGBC.Config
import qualified Machine.GBC.Graphics as Graphics
import qualified SDL
import SDL.Extras
  ( DisplayIndex,
    getCurrentDisplayMode,
    getWindowDisplayIndex,
  )
import qualified SDL.Raw
import System.FilePath (takeFileName)
import qualified Window

-- | Window state.
data WindowContext = WindowContext
  { sdlWindow :: !SDL.Window,
    romFileName :: !FilePath,
    sync :: !Graphics.Sync,
    -- | The SDL display that the current window is centred on.
    displayIndex :: !DisplayIndex,
    framesPerVsync :: !Double,
    -- | Speed relative to full speed (60fps)
    speed :: !Double,
    glState :: !GLState
  }

-- | OpenGL state variables.
data GLState = GLState
  { scaleProgram :: !GL.Program,
    aspectCorrection :: !(StateVar GL.GLmatrix4),
    frameVAO :: !GL.VertexArrayObject,
    frameTexture :: !GL.Texture,
    frameTextureBuffer :: !GL.BufferObject,
    frameTextureBufferBytes :: !(Ptr Word8)
  }

data WindowStatus = Running | Paused | Fault deriving (Eq, Ord, Show)

windowTitle :: FilePath -> WindowStatus -> T.Text
windowTitle romFileName status =
  let prefix = case status of
        Running -> "GBC - "
        Paused -> "GBC *paused* - "
        Fault -> "GBC *fault* - "
   in prefix <> T.pack (takeFileName romFileName)

getFramesPerVsync :: DisplayIndex -> Double -> IO Double
getFramesPerVsync display speed =
  getCurrentDisplayMode display <&> \case
    Nothing -> 1
    Just mode ->
      let rawRefreshRate = SDL.Raw.displayModeRefreshRate mode
          refreshRate = (15 :: Int) * round (fromIntegral rawRefreshRate / 15.0 :: Double)
       in fromIntegral refreshRate / (60.0 * speed)

-- | Initialize a window, and start the rendering thread.
start :: FilePath -> HGBC.Config.Config k Identity -> Graphics.Sync -> IO (Window.Window, Ptr Word8)
start romFileName HGBC.Config.Config {..} sync = do
  let glConfig = SDL.defaultOpenGL {SDL.glProfile = SDL.Core SDL.Normal 4 4}
  sdlWindow <-
    SDL.createWindow
      (windowTitle romFileName Running)
      SDL.defaultWindow
        { SDL.windowInitialSize = fromIntegral <$> SDL.V2 (160 * scale) (144 * scale),
          SDL.windowGraphicsContext = SDL.OpenGLContext glConfig,
          SDL.windowResizable = True
        }
  displayIndex <- getWindowDisplayIndex sdlWindow
  framesPerVsync <- getFramesPerVsync displayIndex speed
  frameBufferPointerRef <- newEmptyMVar
  threadId <- forkOS $ do
    void (SDL.glCreateContext sdlWindow)
    SDL.swapInterval $= if noVsync then SDL.ImmediateUpdates else SDL.SynchronizedUpdates
    glState <- setUpOpenGL
    putMVar frameBufferPointerRef (frameTextureBufferBytes glState)

    mask $ \_ -> eventLoop 0 WindowContext {..}
    SDL.destroyWindow sdlWindow

  frameBufferPointer <- takeMVar frameBufferPointerRef

  SDL.cursorVisible $= False
  pure (Window.new sdlWindow threadId, frameBufferPointer)

-- | The main event loop for the renderer.
--
-- A note on speed control:
--
-- There are two refresh rates to consider, the simulated refresh rate (60fps *
-- speed), and the hardware refresh rate (determined by the user's display
-- hardware). These refresh rates might not divide evenly. As such, the number
-- of frames to render for each simulated frame is a floating point number. We
-- can only render a whole number of frames, so the fractional remainder is
-- accumulated and passed on to the next iteration of the 'eventLoop'.
eventLoop :: Double -> WindowContext -> IO ()
eventLoop extraFrames context@WindowContext {..} = do
  signal <- try (takeMVar (Graphics.signalWindow sync))
  case signal of
    Left Window.Close ->
      -- Drain the signal MVar to prevent the emulator thread from blocking.
      void $ tryTakeMVar (Graphics.signalWindow sync)
    Left (Window.SizeChanged (SDL.V2 w h)) -> do
      GL.glViewport 0 0 w h
      matrix <- aspectCorrectionMatrix w h
      aspectCorrection glState $= matrix
      eventLoop extraFrames context
    Left (Window.Moved _) -> eventLoop extraFrames =<< updateFramesPerSync context
    Left Window.Paused -> do
      SDL.windowTitle sdlWindow $= windowTitle romFileName Paused
      eventLoop extraFrames context
    Left Window.Fault -> do
      SDL.windowTitle sdlWindow $= windowTitle romFileName Fault
      eventLoop extraFrames context
    Left Window.Resumed -> do
      SDL.windowTitle sdlWindow $= windowTitle romFileName Running
      eventLoop extraFrames context
    Right () -> do
      let frames = extraFrames + framesPerVsync

      if frames < 1
        then do
          void $ tryPutMVar (Graphics.bufferAvailable sync) ()
          eventLoop frames context
        else do
          GL.glClear GL.GL_COLOR_BUFFER_BIT
          GL.bindBuffer GL.PixelUpload (frameTextureBuffer glState)
          GL.glFlushMappedBufferRange GL.GL_PIXEL_UNPACK_BUFFER 0 (160 * 144 * 4)
          GL.bindTexture GL.Texture2D (frameTexture glState)
          GL.glTexSubImage2D GL.GL_TEXTURE_2D 0 0 0 160 144 GL.GL_RGBA GL.GL_UNSIGNED_BYTE nullPtr

          extraFrames' <- renderFrames frames
          eventLoop extraFrames' context
  where
    -- Draw as many frames as required
    renderFrames frames
      | frames < 1 = pure frames
      | frames < 2 = do
        -- This is the last frame, so notify that we're done with the buffer.
        GL.glDrawElements GL.GL_TRIANGLES 6 GL.GL_UNSIGNED_INT nullPtr
        GL.glFinish
        void $ tryPutMVar (Graphics.bufferAvailable sync) ()
        SDL.glSwapWindow sdlWindow
        pure (frames - 1)
      | otherwise = do
        -- There is at least one more frame after this one, so put out the frame
        -- quickly and carry one.
        GL.glDrawElements GL.GL_TRIANGLES 6 GL.GL_UNSIGNED_INT nullPtr
        SDL.glSwapWindow sdlWindow
        renderFrames (frames - 1)

-- | Update framesPerVsync based on the current refresh rate.
updateFramesPerSync :: WindowContext -> IO WindowContext
updateFramesPerSync context = do
  display <- getWindowDisplayIndex (sdlWindow context)
  if display == displayIndex context
    then pure context
    else do
      f <- getFramesPerVsync display (speed context)
      pure (context {displayIndex = display, framesPerVsync = f})

-- | Position of the output point.
position :: GL.Attribute
position = GL.Attribute "position" 2 GL.Ints GL.PerVertex GL.KeepInteger

scaleVert :: B.ByteString
scaleVert = $(embedOneFileOf ["sdl/shaders/scale.vert", "shaders/scale.vert"])

scaleFrag :: B.ByteString
scaleFrag = $(embedOneFileOf ["sdl/shaders/scale.frag", "shaders/scale.frag"])

-- | Configure OpenGL.
setUpOpenGL :: IO GLState
setUpOpenGL = do
  scaleProgram <- GL.compileShaders [(GL.VertexShader, scaleVert), (GL.FragmentShader, scaleFrag)]
  GL.useProgram scaleProgram

  frameVAO <- GL.genVertexArrayObject
  GL.bindVertexArrayObject frameVAO
  void (GL.makeVertexBuffer (join [[-1.0, -1.0 :: Float], [1.0, -1.0], [1.0, 1.0], [-1.0, 1.0]]))
  GL.linkAttribute scaleProgram position 0 8
  void (GL.makeElementBuffer (join [[0, 1, 2], [2, 3, 0]]))

  aspectCorrection <- GL.linkUniform scaleProgram "aspectCorrection"
  initialAspectCorrection <- aspectCorrectionMatrix 160 144
  aspectCorrection $= initialAspectCorrection

  GL.activeTextureUnit (GL.TextureUnit 0)
  frameSampler <- GL.linkUniform scaleProgram "frame"
  frameSampler $= GL.TextureUnit 0
  frameTexture <- GL.genTexture
  GL.bindTexture GL.Texture2D frameTexture
  GL.glTexStorage2D GL.GL_TEXTURE_2D 1 GL.GL_RGBA8 160 144
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_NEAREST)
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_NEAREST)

  (frameTextureBuffer, frameTextureBufferBytes) <-
    GL.makeWritablePersistentBuffer
      GL.ExplicitFlush
      GL.PixelUpload
      (160 * 144 * 4)

  pure GLState {..}

aspectCorrectionMatrix :: MonadIO m => GL.GLsizei -> GL.GLsizei -> m GL.GLmatrix4
aspectCorrectionMatrix w h
  | w * 144 == h * 160 = identity
  | w * 144 > h * 160 = tooWide
  | otherwise = tooTall
  where
    wf = fromIntegral w
    hf = fromIntegral h
    wc = 160.0 * hf / 144.0
    hc = 144.0 * wf / 160.0
    identity = GL.makeMatrix ([1, 0, 0, 0] <> [0, 1, 0, 0] <> [0, 0, 1, 0] <> [0, 0, 0, 1])
    tooWide = GL.makeMatrix ([wc / wf, 0, 0, 0] <> [0, 1, 0, 0] <> [0, 0, 1, 0] <> [0, 0, 0, 1])
    tooTall = GL.makeMatrix ([1, 0, 0, 0] <> [0, hc / hf, 0, 0] <> [0, 0, 1, 0] <> [0, 0, 0, 1])
