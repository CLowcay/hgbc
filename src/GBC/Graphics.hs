{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Graphics
  ( VideoBuffers(..)
  , startOutput
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.Int
import           Data.StateVar
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.GraphicsSync
import           GBC.Memory
import           GLUtils
import           Graphics.GL.Core44
import qualified Data.ByteString               as B
import qualified SDL
import qualified SDL.Raw

data WindowContext = WindowContext {
    window :: !SDL.Window
  , sync :: !GraphicsSync
  , glState :: !GLState
  , lineCounter :: !(IORef Int)
  , waitCounter :: !(IORef Int)
}

-- | OpenGL state variables.
data GLState = GLState {
    bgProgram :: !Program
  , bgProjection :: !(StateVar GLmatrix4)
  , bgScanline :: !VertexArrayObject
  , oamProgram :: !Program
  , oamBox :: !VertexArrayObject
}

startOutput :: GraphicsSync -> IO (VideoBuffers, SDL.Window)
startOutput sync = do
  let glConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 4 }
  window <- SDL.createWindow
    "Graphics output"
    SDL.defaultWindow { SDL.windowInitialSize     = SDL.V2 160 144
                      , SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
                      }

  videoBuffers <- newEmptyMVar

  void $ forkOS $ do
    printDisplayModeInfo

    void (SDL.glCreateContext window)
    SDL.swapInterval $= SDL.SynchronizedUpdates 
    (glState, buffers) <- setUpOpenGL
    putMVar videoBuffers buffers

    lineCounter <- newIORef 0
    waitCounter <- newIORef 0

    eventLoop WindowContext { .. }
    SDL.destroyWindow window

  buffers <- takeMVar videoBuffers
  pure (buffers, window)

printDisplayModeInfo :: IO ()
printDisplayModeInfo = alloca $ \pDisplayMode -> do
  void (SDL.Raw.getDesktopDisplayMode 0 pDisplayMode)
  mode <- peek pDisplayMode
  putStrLn
    (  "Display mode "
    ++ show (SDL.Raw.displayModeW mode)
    ++ "x"
    ++ show (SDL.Raw.displayModeH mode)
    ++ " "
    ++ show (SDL.Raw.displayModeRefreshRate mode)
    ++ "Hz"
    )

eventLoop :: WindowContext -> IO ()
eventLoop context@WindowContext {..} = do

  line <- takeMVar (currentLine sync)

  useProgram (oamProgram glState)
  bindVertexArrayObject (oamBox glState)
  glDrawElementsInstanced GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr 40

  useProgram (bgProgram glState)
  bindVertexArrayObject (bgScanline glState)
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr

  glFinish

  putMVar (hblankStart sync) ()

  when (line == 143) $ do
    SDL.glSwapWindow window
    glClear GL_DEPTH_BUFFER_BIT

  unless (line == 255) $ eventLoop context

-- | Position of the scanline.
position :: Attribute
position = Attribute "position" 2 Ints PerVertex KeepInteger

-- | Configure OpenGL.
setUpOpenGL :: IO (GLState, VideoBuffers)
setUpOpenGL = do
  glViewport 0 0 160 144
  glEnable GL_DEPTH_TEST

  gbVert    <- B.readFile "shaders/bg.vert"
  gbFrag    <- B.readFile "shaders/bg.frag"
  bgProgram <- compileShaders [(VertexShader, gbVert), (FragmentShader, gbFrag)]
  useProgram bgProgram

  bgProjection     <- linkUniform bgProgram "projection"
  projectionMatrix <- makeMatrix
    (join [[2.0 / 160, 0, 0, 0], [0, -2.0 / 144, 0, 0], [0, 0, 2 / 20481, 0], [-1, 1, -1, 1]])
  bgProjection $= projectionMatrix

  bgScanline <- genVertexArrayObject
  bindVertexArrayObject bgScanline
  void (makeVertexBuffer (join [[0, 0 :: Int32], [160, 0], [160, 1], [0, 1]]))
  linkAttribute bgProgram position 0 8
  void (makeElementBuffer (join [[0, 1, 2], [2, 3, 0]]))

  (vramBuffer, vram) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer 0x2000
  setUpTextureBuffer bgProgram "texCharacterData"  (TextureUnit 0) vramBuffer 0      0x1800
  setUpTextureBuffer bgProgram "texBackgroundData" (TextureUnit 1) vramBuffer 0x1800 0x800

  (registersBuffer, registers) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer 48
  linkUniformBuffer bgProgram "Registers" registersBuffer 0

  oamVert    <- B.readFile "shaders/oam.vert"
  oamFrag    <- B.readFile "shaders/oam.frag"
  oamProgram <- compileShaders [(VertexShader, oamVert), (FragmentShader, oamFrag)]
  useProgram oamProgram

  oamProjection <- linkUniform oamProgram "projection"
  oamProjection $= projectionMatrix

  oamTexCharacterData  <- linkUniform oamProgram "texCharacterData"
  oamTexCharacterData $= TextureUnit 0

  oamBox <- genVertexArrayObject
  bindVertexArrayObject oamBox
  void (makeVertexBuffer (join [[0, 0 :: Int32], [8, 0], [8, 8], [0, 8]]))
  linkAttribute oamProgram position 0 8
  void (makeElementBuffer (join [[0, 1, 2], [2, 3, 0]]))

  (oamBuffer, oam) <- makeWritablePersistentBuffer Coherent TextureBufferBuffer (40 * 4 * 4)
  linkUniformBuffer oamProgram "Registers"    registersBuffer 0
  linkUniformBuffer oamProgram "OAMRegisters" oamBuffer       1

  pure (GLState { .. }, VideoBuffers { .. })

 where
  setUpTextureBuffer program uniform textureUnit buffer offset size = do
    activeTextureUnit textureUnit
    sampler <- linkUniform program uniform
    sampler $= textureUnit
    texture <- genTexture
    bindTexture TextureBuffer texture
    linkTextureBufferRange buffer offset size
