{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Debugger
  ( start
  , sendNotification
  , Notification(..)
  , DebuggerChannel
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.FileEmbed
import           Data.Functor.Identity
import           Debugger.HTML
import           Debugger.Status
import           Machine.GBC                    ( EmulatorState )
import qualified Config
import qualified Control.Concurrent.Async      as Async
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as LB
import qualified Emulator
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp

data Notification
  = EmulatorStarted
  | EmulatorPaused
  deriving (Eq, Ord, Show)

newtype DebuggerChannel = DebuggerChannel (TChan Notification)

sendNotification :: MonadIO m => DebuggerChannel -> Notification -> m ()
sendNotification (DebuggerChannel channel) = liftIO . atomically . writeTChan channel

start
  :: FilePath -> Config.Config Identity -> Emulator.Emulator -> EmulatorState -> IO DebuggerChannel
start romFileName Config.Config {..} emulator emulatorState = do
  channel <- DebuggerChannel <$> newTChanIO
  void $ forkIO $ Warp.run debugPort (debugger channel romFileName emulator emulatorState)
  pure channel

debugger :: DebuggerChannel -> FilePath -> Emulator.Emulator -> EmulatorState -> Wai.Application
debugger channel romFileName emulator emulatorState req respond =
  respond =<< case Wai.pathInfo req of
    [] -> case Wai.requestMethod req of
      "GET" -> pure
        (Wai.responseLBS HTTP.status200
                         [(HTTP.hContentType, "text/html")]
                         (BB.toLazyByteString (debugHTML romFileName))
        )
      "POST" -> do
        body <- Wai.lazyRequestBody req
        case HTTP.parseQuery (LB.toStrict body) of
          [("run", _)] -> do
            Emulator.sendNotification emulator Emulator.PauseNotification
            pure emptyResponse
          [("step", _)] -> do
            Emulator.sendNotification emulator Emulator.StepNotification
            pure emptyResponse
          [("stepOver", _)] -> do
            Emulator.sendNotification emulator Emulator.StepOverNotification
            pure emptyResponse
          [("stepOut", _)] -> do
            Emulator.sendNotification emulator Emulator.StepOutNotification
            pure emptyResponse
          [("restart", _)] -> do
            Emulator.sendNotification emulator Emulator.RestartNotification
            pure emptyResponse
          query -> do
            putStrLn ("Invalid command: " <> show query)
            pure (httpError HTTP.status422 "Invalid command")
      method -> pure (httpError HTTP.status404 ("Cannot " <> LB.fromStrict method <> " on /"))
    ["css"   ] -> pure debugCSS
    ["js"    ] -> pure debugJS
    ["events"] -> pure (events channel emulatorState)
    _          -> pure debug404
 where
  emptyResponse = Wai.responseLBS HTTP.status200
                                  [(HTTP.hContentType, "text/html")]
                                  "<html><head><meta charset=UTF-8></head></html>"
  httpError status = Wai.responseLBS status [(HTTP.hContentType, "text/plain")]
  debug404 = httpError HTTP.status404 ("No such resource " <> LB.fromStrict (Wai.rawPathInfo req))

keepAliveTime :: Int
keepAliveTime = 60 * 1000000

updateDelay :: Int
updateDelay = 500000

events :: DebuggerChannel -> EmulatorState -> Wai.Response
events (DebuggerChannel channel) emulatorState = Wai.responseStream
  HTTP.status200
  [(HTTP.hContentType, "text/event-stream"), (HTTP.hCacheControl, "no-cache")]
  eventStream

 where
  eventStream :: (BB.Builder -> IO ()) -> IO () -> IO ()
  eventStream write flush = pushPaused >> continue True

   where
    pushStatus = do
      status <- getStatus emulatorState
      write "event: status\n"
      write "data: "
      write (BB.lazyByteString (encode status))
      write "\n\n" >> flush
    pushPaused = do
      write "event: paused\ndata:\n\n" >> flush
      pushStatus
    continue isPaused = do
      event <- Async.race (threadDelay (if isPaused then keepAliveTime else updateDelay))
                          (atomically (readTChan channel))
      case event of
        Left () -> do
          if isPaused then write ": keep-alive\n\n" >> flush else pushStatus
          continue isPaused
        Right EmulatorStarted -> do
          write "event: started\ndata:\n\n" >> flush
          continue False
        Right EmulatorPaused -> do
          pushPaused
          continue True

debugCSS :: Wai.Response
debugCSS = Wai.responseLBS
  HTTP.status200
  [(HTTP.hContentType, "text/css")]
  (LB.fromStrict $(embedOneFileOf ["data/debugger.css","../data/debugger.css"]))

debugJS :: Wai.Response
debugJS = Wai.responseLBS
  HTTP.status200
  [(HTTP.hContentType, "application/javascript")]
  (LB.fromStrict $(embedOneFileOf ["data/debugger.js","../data/debugger.js"]))
