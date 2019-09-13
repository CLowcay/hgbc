{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Debug.TileViewer
  ( startTileViewer
  )
where

import           Control.Concurrent
import           GBC.Memory
import           Control.Monad
import           Control.Monad.Reader
import           Control.Exception
import           GBC.Graphics
import           SDL

startTileViewer :: Memory -> IO (MVar (Maybe Update), Window)
startTileViewer memory = do
  window <- createWindow "Character data viewer" defaultWindow { windowInitialSize = V2 128 192 }
  queue  <- newEmptyMVar
  void $ mask_ $ forkOS $ do
    renderer    <- createRenderer window (-1) defaultRenderer
    tileSurface <- createTexture renderer RGB24 TextureAccessStreaming (V2 128 192)
    eventLoop queue renderer tileSurface memory
    destroyWindow window
  pure (queue, window)

eventLoop :: MVar (Maybe Update) -> Renderer -> Texture -> Memory -> IO ()
eventLoop queue renderer tileSurface memory = go
 where
  go = do
    mupdate <- takeMVar queue
    case mupdate of
      Nothing          -> pure ()
      Just Update {..} -> do
        when updateVRAM $ do
          textureData <- runReaderT decodeVRAM memory
          void $ updateTexture tileSurface Nothing textureData 384
          render
        go

  render = do
    rendererDrawColor renderer $= V4 0xFF 0xFF 0xFF 0xFF
    clear renderer
    copy renderer tileSurface Nothing Nothing
    present renderer
