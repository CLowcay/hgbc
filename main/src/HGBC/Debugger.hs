{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HGBC.Debugger
  ( start,
    restoreLabels,
    restoreBreakpoints,
    DebugState (..),
  )
where

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad
import Control.Monad.Reader
import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified HGBC.Debugger.Breakpoints as Breakpoints
import qualified HGBC.Debugger.Disassembly as Disassembly
import HGBC.Debugger.HTML
import qualified HGBC.Debugger.JSON as JSON
import qualified HGBC.Debugger.Labels as Labels
import HGBC.Debugger.Logging
import qualified HGBC.Debugger.Memory as Memory
import qualified HGBC.Debugger.Resources as Resource
import HGBC.Debugger.State
import HGBC.Debugger.Status
import qualified HGBC.Emulator as Emulator
import HGBC.Errors
import qualified HGBC.Events as Event
import Machine.GBC.CPU (readPC)
import Machine.GBC.Disassembler
import qualified Machine.GBC.Emulator as Emulator
import Machine.GBC.Memory
  ( bootROMLength,
    getBank,
  )
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Text.Read

-- | Start the debugger
start :: Int -> Emulator.RuntimeConfig -> Emulator.State -> IO [FileParseErrors]
start debugPort runtimeState@Emulator.RuntimeConfig {..} emulatorState = do
  r <- restoreState
  void $ forkIO $ Warp.run debugPort (debugger runtimeState emulatorState)
  pure r
  where
    restoreState = do
      w0 <- restoreBreakpoints debugState
      w1 <- restoreLabels debugState
      restoredLabels <- map fst <$> Labels.getAsList debugState
      (disassembly, labels) <- disassembleROM (Emulator.memory emulatorState) restoredLabels
      Disassembly.set debugState disassembly
      Labels.addFromList debugState eventChannel labels
      pure (w0 ++ w1)

debugger :: Emulator.RuntimeConfig -> Emulator.State -> Wai.Application
debugger Emulator.RuntimeConfig {..} emulatorState req respond =
  respond =<< case Wai.pathInfo req of
    [] -> case Wai.requestMethod req of
      "GET" ->
        pure
          ( Wai.responseLBS
              HTTP.status200
              [(HTTP.hContentType, "text/html")]
              (debugHTML romFileName (bootROMLength (Emulator.memory emulatorState)))
          )
      "POST" -> do
        body <- Wai.lazyRequestBody req
        case HTTP.parseQuery (LBC.toStrict body) of
          [("run", _)] -> emptyResponse <$ Emulator.send commandChannel Emulator.Pause
          [("step", _)] -> emptyResponse <$ Emulator.send commandChannel Emulator.Step
          [("stepOver", _)] -> emptyResponse <$ Emulator.send commandChannel Emulator.StepOver
          [("stepOut", _)] -> emptyResponse <$ Emulator.send commandChannel Emulator.StepOut
          [("restart", _)] -> emptyResponse <$ Emulator.send commandChannel Emulator.Restart
          [("runTo", _), ("bank", Just bank), ("offset", Just offset)] ->
            case LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset of
              Nothing -> invalidCommandParamters "runTo"
              Just address ->
                emptyResponse <$ Emulator.send commandChannel (Emulator.RunTo address)
          _ -> invalidCommand ""
      _ -> badMethod
    ["css"] -> cssResponse Resource.css
    ["js"] -> jsResponse Resource.js
    ["svg", "run"] -> svgResponse Resource.svgRun
    ["svg", "pause"] -> svgResponse Resource.svgPause
    ["svg", "step"] -> svgResponse Resource.svgStep
    ["svg", "stepout"] -> svgResponse Resource.svgStepout
    ["svg", "stepthrough"] -> svgResponse Resource.svgStepthrough
    ["svg", "reset"] -> svgResponse Resource.svgReset
    ["svg", "runto"] -> svgResponse Resource.svgRunto
    ["svg", "breakpoint"] -> svgResponse Resource.svgBreakpoint
    ["svg", "breakpoint_disabled"] -> svgResponse Resource.svgBreakpointDisabled
    ["svg", "home"] -> svgResponse Resource.svgHome
    ["svg", "label"] -> svgResponse Resource.svgLabel
    ["svg", "download"] -> svgResponse Resource.svgDownload
    ["memory"] -> whenMethodGET $ case Wai.queryString req of
      [("address", Just addressText), ("lines", Just rawLines)] ->
        case (,) <$> readMaybe ("0x" <> BC.unpack addressText) <*> readMaybe (BC.unpack rawLines) of
          Nothing -> invalidQuery
          Just (address, memLines) ->
            textResponse <$> Memory.memoryAt emulatorState address memLines
      _ -> invalidQuery
    ["backtrace"] -> whenMethodGET $ jsonResponse <$> Memory.backtrace emulatorState
    ["stack"] -> whenMethodGET $ case Wai.queryString req of
      [("offset", Just offsetText), ("n", Just linesText)] ->
        case (,) <$> readMaybeHexText offsetText <*> readMaybeText linesText of
          Nothing -> invalidQuery
          Just (offset, n) -> textResponse <$> Memory.stackAt emulatorState offset n
      _ -> invalidQuery
    ["disassembly"] -> whenMethodGET $ case Wai.queryString req of
      [] -> do
        disassembly <- Disassembly.getAll debugState
        pure
          ( Wai.responseLBS
              HTTP.status200
              [ (HTTP.hContentType, "text/plain; charset=UTF-8"),
                ("Content-Disposition", "attachment"),
                (HTTP.hCacheControl, "no-cache")
              ]
              (LT.encodeUtf8 disassembly)
          )
      [("bank", Just bank), ("offset", Just offset), ("n", Just linesText)] ->
        case (,)
          <$> (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
          <*> readMaybeText linesText of
          Nothing -> invalidQuery
          Just (address, n) -> jsonResponse <$> Disassembly.getN debugState address n
      _ -> invalidQuery
    ["breakpoints"] -> case Wai.requestMethod req of
      "GET" ->
        jsonResponse . JSON.toLazyByteString . JSON.breakpoints <$> Breakpoints.getAsList debugState
      "POST" -> withAddress $ \address -> do
        body <- Wai.lazyRequestBody req
        case HTTP.parseQuery (LBC.toStrict body) of
          [("set", _)] -> emptyResponse <$ Breakpoints.set debugState eventChannel address
          [("disable", _)] -> emptyResponse <$ Breakpoints.disable debugState eventChannel address
          [("unset", _)] -> emptyResponse <$ Breakpoints.unset debugState eventChannel address
          _ -> invalidCommand "breakpoints"
      _ -> badMethod
    ["labels"] ->
      whenMethodGET
        (jsonResponse . JSON.toLazyByteString . JSON.labels <$> Labels.getAsList debugState)
    ["label"] -> withAddress $ \address -> whenMethodPOST $ do
      body <- Wai.lazyRequestBody req
      case HTTP.parseQuery (LBC.toStrict body) of
        [("update", Just rawText)] ->
          emptyResponse
            <$ Labels.update
              debugState
              eventChannel
              address
              (T.decodeUtf8With T.lenientDecode rawText)
        [("delete", _)] -> emptyResponse <$ Labels.delete debugState eventChannel address
        _ -> invalidCommand "label"
    ["events"] ->
      whenMethodGET
        ( pure
            ( Wai.responseStream
                HTTP.status200
                [(HTTP.hContentType, "text/event-stream"), (HTTP.hCacheControl, "no-cache")]
                (eventStream eventChannel emulatorState)
            )
        )
    _ -> resourceNotFound
  where
    emptyResponse =
      Wai.responseLBS
        HTTP.status200
        [(HTTP.hContentType, "text/html")]
        "<html><head><meta charset=UTF-8></head></html>"
    httpError status message = do
      logError req (LBC.unpack message)
      pure (Wai.responseLBS status [(HTTP.hContentType, "text/plain")] message)
    badMethod = httpError HTTP.status405 "Method not supported"
    resourceNotFound = httpError HTTP.status404 "No such resource"
    invalidCommandParamters resource =
      httpError HTTP.status422 ("Invalid parameters for " <> resource <> " command")
    invalidCommand resource =
      httpError
        HTTP.status422
        (if LBC.null resource then "Invalid command" else "Invalid " <> resource <> " command")
    invalidQuery = httpError HTTP.status422 "Invalid query string"
    readMaybeHexText t = readMaybe ("0x" <> BC.unpack t)
    readMaybeText t = readMaybe (BC.unpack t)
    whenMethodGET handler = if Wai.requestMethod req == "GET" then handler else badMethod
    whenMethodPOST handler = if Wai.requestMethod req == "POST" then handler else badMethod
    withAddress handler = case Wai.queryString req of
      [("bank", Just bank), ("offset", Just offset)] ->
        maybe invalidQuery handler (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
      _ -> invalidQuery

    textResponse =
      Wai.responseLBS
        HTTP.status200
        [(HTTP.hContentType, "text/plain"), (HTTP.hCacheControl, "no-cache")]

    jsonResponse =
      Wai.responseLBS
        HTTP.status200
        [(HTTP.hContentType, "application/json"), (HTTP.hCacheControl, "no-cache")]

    svgResponse content =
      whenMethodGET
        (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "image/svg+xml")] content))
    cssResponse content =
      whenMethodGET (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/css")] content))
    jsResponse content =
      whenMethodGET
        (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/javascript")] content))

-- | Minimum frequency to send a keep-alive event.
keepAliveTime :: Int
keepAliveTime = 60 * 1000000

-- | Delay between updates.
updateDelay :: Int
updateDelay = 250000

eventStream :: Event.Channel -> Emulator.State -> (BB.Builder -> IO ()) -> IO () -> IO ()
eventStream channel emulatorState write flush = do
  waitEvent <- Event.waitAction channel
  let continue isPaused = do
        event <- Async.race (threadDelay (if isPaused then keepAliveTime else updateDelay)) waitEvent
        case event of
          Left () -> do
            if isPaused then write ": keep-alive\n\n" >> flush else pushStatus
            continue isPaused
          Right Event.Resumed -> do
            write "event: started\ndata:\n\n" >> flush
            continue False
          Right Event.Paused -> do
            pushPaused
            continue True
          Right (Event.BreakPointSet address) -> do
            write "event: breakpoint-added\ndata:"
            pushAddress address
            continue isPaused
          Right (Event.BreakPointDisabled address) -> do
            write "event: breakpoint-disabled\ndata:"
            pushAddress address
            continue isPaused
          Right (Event.BreakPointRemoved address) -> do
            write "event: breakpoint-removed\ndata:"
            pushAddress address
            continue isPaused
          Right (Event.LabelUpdated labels) -> do
            write ("event: label-added\ndata:" <> JSON.fromEncoding (JSON.labels labels) <> "\n\n")
            flush
            continue isPaused
          Right (Event.LabelRemoved address) -> do
            write "event: label-removed\ndata:"
            pushAddress address
            continue isPaused
          _ -> continue isPaused

  pushPaused >> continue True
  where
    pushStatus = do
      status <- getStatus emulatorState
      write ("event: status\ndata:" <> BB.lazyByteString status <> "\n\n") >> flush
    pushPaused = do
      pushStatus
      (bank, pc) <-
        runReaderT
          ( do
              pc' <- readPC
              bank' <- getBank pc'
              pure (bank', pc')
          )
          emulatorState
      write
        ( "event: paused\ndata:{\"bank\":"
            <> fromString (show bank)
            <> ",\"offset\":"
            <> fromString (show pc)
            <> "}\n\n"
        )
        >> flush
    pushAddress address = write (JSON.fromEncoding (JSON.longAddress address) <> "\n\n") >> flush
