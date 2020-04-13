{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Debugger
  ( start
  , sendNotification
  , Notification(..)
  , DebuggerChannel
  , DebugState(..)
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
import           Data.Maybe
import           Data.String
import           Data.Traversable
import           Data.Word
import           Debugger.Disassemble
import           Debugger.HTML
import           Debugger.Status
import           Machine.GBC                    ( EmulatorState )
import           Machine.GBC.CPU                ( readPC )
import           Machine.GBC.Memory             ( readChunk
                                                , getBank
                                                )
import           Machine.GBC.Util               ( formatHex )
import           Text.Read
import qualified Config
import qualified Control.Concurrent.Async      as Async
import qualified Data.Binary.Builder           as BNB
import qualified Data.ByteString               as B
import           Debugger.Types
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as CB
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.HashTable.IO             as H
import qualified Emulator
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp

data Notification
  = EmulatorStarted
  | EmulatorPaused
  | BreakPointAdded LongAddress
  | BreakPointRemoved LongAddress
  deriving (Eq, Ord, Show)

newtype DebuggerChannel = DebuggerChannel (TChan Notification)

sendNotification :: MonadIO m => DebuggerChannel -> Notification -> m ()
sendNotification (DebuggerChannel channel) = liftIO . atomically . writeTChan channel

data DebugState = DebugState {
    disassemblyRef :: IORef Disassembly
  , breakPoints    :: H.BasicHashTable LongAddress ()
}

start
  :: FilePath
  -> Config.Config Identity
  -> Emulator.Emulator
  -> EmulatorState
  -> DebugState
  -> IO DebuggerChannel
start romFileName Config.Config {..} emulator emulatorState debugState = do
  channel <- DebuggerChannel <$> newTChanIO
  void $ forkIO $ Warp.run debugPort
                           (debugger channel romFileName emulator emulatorState debugState)
  pure channel

debugger
  :: DebuggerChannel
  -> FilePath
  -> Emulator.Emulator
  -> EmulatorState
  -> DebugState
  -> Wai.Application
debugger channel romFileName emulator emulatorState debugState req respond =
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
          [("runTo", _), ("bank", Just bank), ("offset", Just offset)] ->
            case LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset of
              Nothing -> do
                putStrLn ("Invalid parameters for runTo command: " <> show body)
                pure (httpError HTTP.status422 "invalid parameters")
              Just address -> do
                Emulator.sendNotification emulator (Emulator.RunToNotification address)
                pure emptyResponse
          query -> do
            putStrLn ("Invalid command: " <> show query)
            pure (httpError HTTP.status422 "Invalid command")
      method -> pure (httpError HTTP.status404 ("Cannot " <> LB.fromStrict method <> " on /"))

    ["css"] -> pure debugCSS
    ["js" ] -> pure debugJS
    ["svg", "run"] ->
      pure (svg (LB.fromStrict $(embedOneFileOf ["data/play.svg", "../data/play.svg"])))
    ["svg", "pause"] ->
      pure (svg (LB.fromStrict $(embedOneFileOf ["data/pause.svg", "../data/pause.svg"])))
    ["svg", "step"] ->
      pure (svg (LB.fromStrict $(embedOneFileOf ["data/step.svg", "../data/step.svg"])))
    ["svg", "stepout"] ->
      pure (svg (LB.fromStrict $(embedOneFileOf ["data/stepout.svg", "../data/stepout.svg"])))
    ["svg", "stepthrough"] -> pure
      (svg (LB.fromStrict $(embedOneFileOf ["data/stepthrough.svg", "../data/stepthrough.svg"])))
    ["svg", "reset"] -> pure
      (svg (LB.fromStrict $(embedOneFileOf ["data/reset.svg", "../data/reset.svg"])))

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
      [("bank", Just bank), ("offset", Just offset), ("n", Just linesText)] ->
        case
            (,)
            <$> (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
            <*> readMaybeText linesText
          of
            Nothing -> do
              putStrLn ("Invalid parameters for /disassembly: " <> show (Wai.queryString req))
              pure (httpError HTTP.status422 "invalid parameters")
            Just (address, n) -> do
              disassembly <- readIORef (disassemblyRef debugState)
              let fields = lookupN disassembly n address
              breakpoints <- filterM (fmap isJust . H.lookup (breakPoints debugState))
                                     (fieldAddress <$> fields)
              pure
                (Wai.responseLBS
                  HTTP.status200
                  [(HTTP.hContentType, "application/json")]
                  ( BNB.toLazyByteString
                  . fromEncoding
                  . pairs
                  . mconcat
                  $ ["fields" .= fields, "breakpoints" .= breakpoints]
                  )
                )
      query -> do
        putStrLn ("Invalid query string for /disassembly: " <> show query)
        pure (httpError HTTP.status422 "invalid query string")

    ["breakpoints"] -> case Wai.queryString req of
      [("bank", Just bank), ("offset", Just offset)] ->
        case LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset of
          Nothing -> do
            putStrLn ("Invalid parameters for /breakpoints: " <> show (Wai.queryString req))
            pure (httpError HTTP.status422 "invalid parameters")
          Just address -> case Wai.requestMethod req of
            "POST" -> do
              body <- Wai.lazyRequestBody req
              case HTTP.parseQuery (LB.toStrict body) of
                [("set", _)] -> do
                  H.insert (breakPoints debugState) address ()
                  sendNotification channel (BreakPointAdded address)
                  pure emptyResponse
                [("unset", _)] -> do
                  H.delete (breakPoints debugState) address
                  sendNotification channel (BreakPointRemoved address)
                  pure emptyResponse
                query -> do
                  putStrLn ("Invalid breakpoint command: " <> show query)
                  pure (httpError HTTP.status422 "Invalid breakpoint command")
            method ->
              pure
                (httpError HTTP.status404 ("Cannot " <> LB.fromStrict method <> " on /breakpoints"))
      query -> do
        putStrLn ("Invalid query string for /breakpoints: " <> show query)
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
      write "event: status\ndata:"
      write . BB.lazyByteString =<< getStatus emulatorState
      write "\n\n" >> flush
    pushPaused = do
      pushStatus
      (bank, pc) <- runReaderT
        (do
          pc'   <- readPC
          bank' <- getBank pc'
          pure (bank', pc')
        )
        emulatorState
      write
          (  "event: paused\ndata:{\"bank\":"
          <> fromString (show bank)
          <> ",\"offset\":"
          <> fromString (show pc)
          <> "}\n\n"
          )
        >> flush
    pushAddress address = do
      write (BB.lazyByteString (encode address))
      write "\n\n" >> flush
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
        Right (BreakPointAdded address) -> do
          write "event: breakpoint-added\ndata:"
          pushAddress address
          continue isPaused
        Right (BreakPointRemoved address) -> do
          write "event: breakpoint-removed\ndata:"
          pushAddress address
          continue isPaused

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

svg :: LB.ByteString -> Wai.Response
svg = Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "image/svg+xml")]

getMemoryAt :: Word16 -> Word16 -> EmulatorState -> IO LBC.ByteString
getMemoryAt address memLines emulatorState = do
  chunks <- runReaderT (for [0 .. (memLines - 1)] $ \i -> readChunk (address + (8 * i)) 8)
                       emulatorState
  pure (LBC.intercalate "\n" (formatChunk <$> chunks))
  where formatChunk s = LBC.intercalate " " $ LBC.pack . formatHex <$> B.unpack s
