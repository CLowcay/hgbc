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
import           GBC.Bus.Synchronizer
import           SDL
import qualified Data.ByteString as B

startTileViewer :: Synchronizer Update -> Memory -> IO ()
startTileViewer synchronizer memory = void $ mask_ $ forkOSWithUnmask $ \unmask ->
  unmask windowAction `finally` notifyThreadTerminated synchronizer
 where
  windowAction = do
    window <- createWindow "Character data viewer" defaultWindow { windowInitialSize = V2 128 192 }
    renderer <- createRenderer window (-1) defaultRenderer
    tileSurface <- createTexture renderer RGB24 TextureAccessStreaming (V2 128 192)
    eventLoop synchronizer renderer tileSurface memory
    destroyWindow window

eventLoop :: Synchronizer Update -> Renderer -> Texture -> Memory -> IO ()
eventLoop synchronizer renderer tileSurface memory = go
 where
  go = do
    update <- getUpdate synchronizer
    case update of
      Nothing          -> pure ()
      Just Update {..} -> do
        textureData <- runReaderT decodeVRAM memory
        void $ updateTexture tileSurface Nothing textureData 384

    event <- pollEvent
    let doQuit = case event of
          Just (Event _ QuitEvent) -> True
          _                        -> False

    if doQuit then pure () else render >> go

  render = do
    rendererDrawColor renderer $= V4 0xFF 0xFF 0xFF 0xFF
    clear renderer
    copy renderer tileSurface Nothing Nothing
    present renderer
