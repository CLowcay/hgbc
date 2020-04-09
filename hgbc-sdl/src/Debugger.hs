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
import           Control.Monad.Reader
import           Data.Aeson
import           Data.FileEmbed
import           Data.Functor.Identity
import           Data.IORef
import           Data.Traversable
import           Data.Word
import           Debugger.Disassemble
import           Debugger.HTML
import           Debugger.Status
import           Machine.GBC                    ( EmulatorState )
import           Machine.GBC.Memory             ( readChunk )
import           Machine.GBC.Util               ( formatHex )
import           Text.Read
import qualified Config
import qualified Control.Concurrent.Async      as Async
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as CB
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC
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
  :: FilePath
  -> Config.Config Identity
  -> Emulator.Emulator
  -> EmulatorState
  -> IORef Disassembly
  -> IO DebuggerChannel
start romFileName Config.Config {..} emulator emulatorState disassemblyRef = do
  channel <- DebuggerChannel <$> newTChanIO
  void $ forkIO $ Warp.run debugPort
                           (debugger channel romFileName emulator emulatorState disassemblyRef)
  pure channel

debugger
  :: DebuggerChannel
  -> FilePath
  -> Emulator.Emulator
  -> EmulatorState
  -> IORef Disassembly
  -> Wai.Application
debugger channel romFileName emulator emulatorState disassemblyRef req respond =
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
    ["memory"] -> case Wai.queryString req of
      [("address", Just addressText), ("lines", Just rawLines)] ->
        case (,) <$> readMaybe ("0x" <> CB.unpack addressText) <*> readMaybe (CB.unpack rawLines) of
          Nothing -> do
            putStrLn ("Invalid parameters for /memory: " <> show (Wai.queryString req))
            pure (httpError HTTP.status422 "invalid parameters")
          Just (address, memLines) ->
            Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")]
              <$> getMemoryAt address memLines emulatorState
      query -> do
        putStrLn ("Invalid query string for /memory: " <> show query)
        pure (httpError HTTP.status422 "invalid query string")
    ["disassembly"] -> case Wai.queryString req of
      [("bank", Just bankText), ("offset", Just offsetText), ("n", Just linesText)] ->
        case
            (,,)
            <$> readMaybeHexText bankText
            <*> readMaybeHexText offsetText
            <*> readMaybeText linesText
          of
            Nothing -> do
              putStrLn ("Invalid parameters for /disassembly: " <> show (Wai.queryString req))
              pure (httpError HTTP.status422 "invalid parameters")
            Just (bank, offset, n) -> do
              disassembly <- readIORef disassemblyRef
              let fields = lookupN disassembly n (LongAddress bank offset)
              pure
                (Wai.responseLBS HTTP.status200
                                 [(HTTP.hContentType, "application/json")]
                                 (encode fields)
                )
      query -> do
        putStrLn ("Invalid query string for /disassembly: " <> show query)
        pure (httpError HTTP.status422 "invalid query string")
    ["events"] -> pure (events channel emulatorState)
    path       -> do
      putStrLn ("No such resource  " <> show path)
      pure (httpError HTTP.status404 ("No such resource " <> LB.fromStrict (Wai.rawPathInfo req)))

 where
  emptyResponse = Wai.responseLBS HTTP.status200
                                  [(HTTP.hContentType, "text/html")]
                                  "<html><head><meta charset=UTF-8></head></html>"
  httpError status = Wai.responseLBS status [(HTTP.hContentType, "text/plain")]
  readMaybeHexText t = readMaybe ("0x" <> CB.unpack t)
  readMaybeText t = readMaybe (CB.unpack t)

keepAliveTime :: Int
keepAliveTime = 60 * 1000000

updateDelay :: Int
updateDelay = 250000

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
      pushStatus
      write "event: paused\ndata:\n\n" >> flush
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

getMemoryAt :: Word16 -> Word16 -> EmulatorState -> IO LBC.ByteString
getMemoryAt address memLines emulatorState = do
  chunks <- runReaderT (for [0 .. (memLines - 1)] $ \i -> readChunk (address + (8 * i)) 8)
                       emulatorState
  pure (LBC.intercalate "\n" (formatChunk <$> chunks))
  where formatChunk s = LBC.intercalate " " $ LBC.pack . formatHex <$> B.unpack s
