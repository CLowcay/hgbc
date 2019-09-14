{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TileViewer
  ( startTileViewer
  )
where

import           Control.Concurrent
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Control.Monad
import           Control.Monad.Reader
import           GBC.Graphics
import           GBC.Memory
import           SDL

data WindowContext = WindowContext {
    renderer :: Renderer
  , tileSurface :: Texture
  , memory :: Memory
  , queue :: MVar (Maybe Update)
}

instance HasMemory WindowContext where
  {-# INLINE forMemory #-}
  forMemory = memory

startTileViewer :: UsesMemory env m => ReaderT env m (MVar (Maybe Update), Window)
startTileViewer = do
  window <- liftIO
    $ createWindow "Character data viewer" defaultWindow { windowInitialSize = V2 128 192 }
  queue  <- liftIO newEmptyMVar
  memory <- asks forMemory
  void $ liftIO $ forkOS $ do
    renderer    <- createRenderer window (-1) defaultRenderer
    tileSurface <- createTexture renderer RGB24 TextureAccessStreaming (V2 128 192)
    runReaderT eventLoop $ WindowContext renderer tileSurface memory queue
    destroyWindow window
  pure (queue, window)

eventLoop :: ReaderT WindowContext IO ()
eventLoop = do
  WindowContext {..} <- ask
  mupdate            <- liftIO $ takeMVar queue
  case mupdate of
    Nothing          -> pure ()
    Just Update {..} -> do
      when updateVRAM $ do
        textureData <- decodeVRAM
        (ptx, _)    <- lockTexture tileSurface Nothing
        liftIO $ pokeArray (castPtr ptx) (concatMap (replicate 3) $ concat textureData)
        unlockTexture tileSurface
        render
      eventLoop

 where
  render = do
    WindowContext {..} <- ask
    clear renderer
    copy renderer tileSurface Nothing Nothing
    present renderer
