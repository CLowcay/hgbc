{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Graphics.Window
  ( startOutput
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.StateVar
import           Data.Word
import           Foreign.Ptr
import           GBC.Graphics
import           GLUtils
import           Graphics.GL.Core44
import qualified Data.ByteString               as B
import qualified SDL

-- | Window state.
data WindowContext = WindowContext {
    window  :: !SDL.Window
  , sync    :: !GraphicsSync
  , glState :: !GLState
}

-- | OpenGL state variables.
data GLState = GLState {
    scaleProgram            :: !Program
  , frameVAO                :: !VertexArrayObject
  , frameTexture            :: !Texture
  , frameTextureBuffer      :: !BufferObject
  , frameTextureBufferBytes :: !(Ptr Word8)
}

-- | Initialize a window, and start the rendering thread.
startOutput :: GraphicsSync -> IO (SDL.Window, Ptr Word8)
startOutput sync = do
  let glConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 4 }
  window <- SDL.createWindow
    "Graphics output"
    SDL.defaultWindow { SDL.windowInitialSize     = SDL.V2 320 288
                      , SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
                      }

  frameBufferPointerRef <- newEmptyMVar
  void $ forkOS $ do
    void (SDL.glCreateContext window)
    SDL.swapInterval $= SDL.SynchronizedUpdates
    glState <- setUpOpenGL
    putMVar frameBufferPointerRef (frameTextureBufferBytes glState)

    eventLoop WindowContext { .. }
    SDL.destroyWindow window

  frameBufferPointer <- takeMVar frameBufferPointerRef
  pure (window, frameBufferPointer)

-- | The main event loop for the renderer.
eventLoop :: WindowContext -> IO ()
eventLoop context@WindowContext {..} = do
  command <- takeMVar (signalWindow sync)
  case command of
    Quit   -> pure ()
    Redraw -> do
      bindBuffer PixelUpload (frameTextureBuffer glState)
      glFlushMappedBufferRange GL_PIXEL_UNPACK_BUFFER 0 (160 * 144 * 4)
      bindTexture Texture2D (frameTexture glState)
      glTexSubImage2D GL_TEXTURE_2D 0 0 0 160 144 GL_RGBA GL_UNSIGNED_BYTE nullPtr
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
      glFinish
      putMVar (bufferAvailable sync) ()

      SDL.glSwapWindow window
      eventLoop context

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
