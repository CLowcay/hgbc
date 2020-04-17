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
import           Data.Aeson.Encoding            ( list )
import           Data.FileEmbed
import           Data.Functor.Identity
import           Data.IORef
import           Data.Maybe
import           Data.String
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Traversable
import           Data.Word
import           Debugger.HTML
import           Debugger.Status
import           Disassembler
import           Machine.GBC                    ( EmulatorState )
import           Machine.GBC.CPU                ( readPC )
import           Machine.GBC.Memory             ( readChunk
                                                , getBank
                                                )
import           Machine.GBC.Util               ( formatHex )
import           System.IO
import           Text.Read
import qualified Config
import qualified Control.Concurrent.Async      as Async
import qualified Data.Binary.Builder           as BNB
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as CB
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashTable.IO             as H
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import qualified Emulator
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp

data Notification
  = EmulatorStarted
  | EmulatorPaused
  | BreakPointAdded LongAddress
  | BreakPointRemoved LongAddress
  | LabelUpdated LongAddress T.Text
  | LabelRemoved LongAddress
  deriving (Eq, Ord, Show)

newtype DebuggerChannel = DebuggerChannel (TChan Notification)

sendNotification :: MonadIO m => DebuggerChannel -> Notification -> m ()
sendNotification (DebuggerChannel channel) = liftIO . atomically . writeTChan channel

data DebugState = DebugState {
    disassemblyRef :: IORef Disassembly
  , breakpoints    :: H.BasicHashTable LongAddress ()
  , labels         :: IORef (HM.HashMap LongAddress T.Text)
}

logError :: Wai.Request -> String -> IO ()
logError req message = do
  time <- getZonedTime
  hPutStrLn
    stderr
    (  "["
    <> formatTime defaultTimeLocale "%F %T%3Q UTC%z" time
    <> "] "
    <> (CB.unpack (Wai.requestMethod req) <> " ")
    <> (CB.unpack (Wai.rawPathInfo req) <> ": ")
    <> message
    )

start
  :: FilePath
  -> Config.Config Identity
  -> Emulator.Emulator
  -> EmulatorState
  -> DebugState
  -> IO DebuggerChannel
start romFileName Config.Config {..} emulator emulatorState debugState = do
  channel <- DebuggerChannel <$> newBroadcastTChanIO
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
              Nothing      -> invalidCommandParamters "runTo"
              Just address -> do
                Emulator.sendNotification emulator (Emulator.RunToNotification address)
                pure emptyResponse
          _ -> invalidCommand ""
      _ -> badMethod

    ["css"] -> whenMethodGET (pure debugCSS)
    ["js" ] -> whenMethodGET (pure debugJS)
    ["svg", "run"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/play.svg", "../data/play.svg"]))
    ["svg", "pause"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/pause.svg", "../data/pause.svg"]))
    ["svg", "step"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/step.svg", "../data/step.svg"]))
    ["svg", "stepout"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/stepout.svg", "../data/stepout.svg"]))
    ["svg", "stepthrough"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/stepthrough.svg", "../data/stepthrough.svg"]))
    ["svg", "reset"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/reset.svg", "../data/reset.svg"]))
    ["svg", "runto"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/runto.svg", "../data/runto.svg"]))
    ["svg", "breakpoint"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/breakpoint.svg", "../data/breakpoint.svg"]))
    ["svg", "home"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/home.svg", "../data/home.svg"]))
    ["svg", "label"] ->
      getSVG (LB.fromStrict $(embedOneFileOf ["data/label.svg", "../data/label.svg"]))

    ["memory"] -> whenMethodGET $ case Wai.queryString req of
      [("address", Just addressText), ("lines", Just rawLines)] ->
        case (,) <$> readMaybe ("0x" <> CB.unpack addressText) <*> readMaybe (CB.unpack rawLines) of
          Nothing -> invalidQuery
          Just (address, memLines) ->
            Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")]
              <$> getMemoryAt address memLines emulatorState
      _ -> invalidQuery

    ["disassembly"] -> whenMethodGET $ case Wai.queryString req of
      [("bank", Just bank), ("offset", Just offset), ("n", Just linesText)] ->
        case
            (,)
            <$> (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
            <*> readMaybeText linesText
          of
            Nothing           -> invalidQuery
            Just (address, n) -> do
              disassembly <- readIORef (disassemblyRef debugState)
              let fields = lookupN disassembly n address
              breakpoints <- filterM (fmap isJust . H.lookup (breakpoints debugState))
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
      _ -> invalidQuery

    ["breakpoints"] -> withAddress $ \address -> whenMethodPOST $ do
      body <- Wai.lazyRequestBody req
      case HTTP.parseQuery (LB.toStrict body) of
        [("set", _)] -> do
          H.insert (breakpoints debugState) address ()
          sendNotification channel (BreakPointAdded address)
          pure emptyResponse
        [("unset", _)] -> do
          H.delete (breakpoints debugState) address
          sendNotification channel (BreakPointRemoved address)
          pure emptyResponse
        _ -> invalidCommand "breakpoints"

    ["labels"] -> whenMethodGET $ do
      allLabels <- HM.toList <$> readIORef (labels debugState)
      let labelsJSON =
            list (\(address, text) -> pairs ("address" .= address <> "text" .= text)) allLabels
      pure
        (Wai.responseLBS HTTP.status200
                         [(HTTP.hContentType, "application/json")]
                         (BNB.toLazyByteString (fromEncoding labelsJSON))
        )

    ["label"] -> withAddress $ \address -> whenMethodPOST $ do
      body <- Wai.lazyRequestBody req
      case HTTP.parseQuery (LB.toStrict body) of
        [("update", Just rawText)] -> do
          let text = T.decodeUtf8With T.lenientDecode rawText
          modifyIORef' (labels debugState) (HM.insert address text)
          sendNotification channel (LabelUpdated address text)
          pure emptyResponse
        [("delete", _)] -> do
          modifyIORef' (labels debugState) (HM.delete address)
          sendNotification channel (LabelRemoved address)
          pure emptyResponse
        _ -> invalidCommand "label"

    ["events"] -> whenMethodGET (pure (events channel emulatorState))
    _          -> resourceNotFound

 where
  emptyResponse = Wai.responseLBS HTTP.status200
                                  [(HTTP.hContentType, "text/html")]
                                  "<html><head><meta charset=UTF-8></head></html>"
  httpError status message = do
    logError req (LBC.unpack message)
    pure (Wai.responseLBS status [(HTTP.hContentType, "text/plain")] message)
  badMethod        = httpError HTTP.status405 "Method not supported"
  resourceNotFound = httpError HTTP.status404 "No such resource"
  invalidCommandParamters resource =
    httpError HTTP.status422 ("Invalid parameters for " <> resource <> " command")
  invalidCommand resource = httpError
    HTTP.status422
    (if LBC.null resource then "Invalid command" else "Invalid " <> resource <> " command")
  invalidQuery = httpError HTTP.status422 "Invalid query string"
  readMaybeHexText t = readMaybe ("0x" <> CB.unpack t)
  readMaybeText t = readMaybe (CB.unpack t)
  whenMethodGET handler = if Wai.requestMethod req == "GET" then handler else badMethod
  whenMethodPOST handler = if Wai.requestMethod req == "POST" then handler else badMethod
  withAddress handler = case Wai.queryString req of
    [("bank", Just bank), ("offset", Just offset)] ->
      maybe invalidQuery handler (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
    _ -> invalidQuery

  getSVG :: LB.ByteString -> IO Wai.Response
  getSVG content = whenMethodGET
    (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "image/svg+xml")] content))

keepAliveTime :: Int
keepAliveTime = 60 * 1000000

updateDelay :: Int
updateDelay = 250000

events :: DebuggerChannel -> EmulatorState -> Wai.Response
events (DebuggerChannel writeChannel) emulatorState = Wai.responseStream
  HTTP.status200
  [(HTTP.hContentType, "text/event-stream"), (HTTP.hCacheControl, "no-cache")]
  eventStream

 where
  eventStream :: (BB.Builder -> IO ()) -> IO () -> IO ()
  eventStream write flush = do
    channel <- atomically (dupTChan writeChannel)
    let continue isPaused = do
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
            Right (LabelUpdated address text) -> do
              write "event: label-added\ndata:"
              write
                (  "{\"address\":"
                <> BB.lazyByteString (encode address)
                <> ",\"text\":"
                <> fromString (show text)
                <> "}\n\n"
                )
              flush
              continue isPaused
            Right (LabelRemoved address) -> do
              write "event: label-removed\ndata:"
              pushAddress address
              continue isPaused

    pushPaused >> continue True

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
