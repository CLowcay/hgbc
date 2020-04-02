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

import           Prelude                 hiding ( head )

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.Functor.Identity
import           Data.String
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

start :: FilePath -> Config.Config Identity -> Emulator.Emulator -> IO DebuggerChannel
start romFileName Config.Config {..} emulator = do
  channel <- DebuggerChannel <$> newTChanIO
  void $ forkIO $ Warp.run debugPort (debugger channel romFileName emulator)
  pure channel

debugger :: DebuggerChannel -> FilePath -> Emulator.Emulator -> Wai.Application
debugger channel romFileName emulator req respond = respond =<< case Wai.pathInfo req of
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
        query -> do
          putStrLn ("Invalid command: " <> show query)
          pure (httpError HTTP.status422 "Invalid command")
    method -> pure (httpError HTTP.status404 ("Cannot " <> LB.fromStrict method <> " on /"))
  ["css"   ] -> pure debugCSS
  ["js"    ] -> pure debugJS
  ["events"] -> pure (events channel)
  _          -> pure debug404
 where
  emptyResponse = Wai.responseLBS HTTP.status200
                                  [(HTTP.hContentType, "text/html")]
                                  "<html><head><meta charset=UTF-8></head></html>"
  httpError status = Wai.responseLBS status [(HTTP.hContentType, "text/plain")]
  debug404 = httpError HTTP.status404 ("No such resource " <> LB.fromStrict (Wai.rawPathInfo req))

eventKeepAliveTime :: Int
eventKeepAliveTime = 60 * 1000000

events :: DebuggerChannel -> Wai.Response
events (DebuggerChannel channel) =
  Wai.responseStream HTTP.status200
                     [(HTTP.hContentType, "text/event-stream"), (HTTP.hCacheControl, "no-cache")]
    $ \write flush -> forever $ do
        event <- Async.race (threadDelay eventKeepAliveTime) (atomically (readTChan channel))
        write $ case event of
          Left  ()              -> ": keep alive"
          Right EmulatorStarted -> "data: {\"status\":\"started\"}"
          Right EmulatorPaused  -> "data: {\"status\":\"paused\"}"
        write "\n\n"
        flush

debugHTML :: FilePath -> BB.Builder
debugHTML romFileName = html [header, main]
 where
  header = head
    [ title (fromString romFileName <> " - hgbc debugger")
    , charset "UTF-8"
    , meta "application-name" "hgbc debugger"
    , link "stylesheet" "css"
    , script "js"
    ]
  main = body
    [ iframe "invisible_frame"
    , nav
      [ form
          HTTP.methodPost
          "/"
          "invisible_frame"
          [ button "run"      "Run"
          , button "step"     "Step"
          , button "stepOver" "Step Over"
          , button "stepOut"  "Step Out"
          ]
      ]
    ]

  html contents = "<!DOCTYPE html><html>" <> mconcat contents <> "</html>"
  head heads = "<head>" <> mconcat heads <> "</head>"
  title t = "<title>" <> t <> "</title>"
  charset c = "<meta charset='" <> c <> "'>"
  meta name content = "<meta name='" <> name <> "' content='" <> content <> "'>"
  link rel href = "<link rel='" <> rel <> "' href='" <> href <> "'>"
  script src = "<script src='" <> src <> "'></script>"
  body contents = "<body>" <> mconcat contents <> "</body>"
  nav contents = "<nav>" <> mconcat contents <> "</nav>"
  iframe name = "<iframe name='" <> name <> "'></iframe>"
  form method action target contents =
    "<form method="
      <> BB.byteString method
      <> " action='"
      <> action
      <> "' target='"
      <> target
      <> "'>"
      <> mconcat contents
      <> "</form>"
  button name content = "<button name='" <> name <> "'>" <> content <> "</button>"

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
