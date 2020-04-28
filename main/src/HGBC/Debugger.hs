{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger
  ( start
  , restoreLabels
  , restoreBreakpoints
  , DebugState(..)
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Functor.Identity
import           HGBC.Debugger.HTML
import           HGBC.Debugger.Logging
import           HGBC.Debugger.State
import           Machine.GBC                    ( EmulatorState(..) )
import           Machine.GBC.Disassembler
import           Machine.GBC.Memory             ( bootROMLength )
import           Text.Read
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import qualified Data.Text.Lazy.Encoding       as LT
import qualified HGBC.Config
import qualified HGBC.Debugger.Breakpoints     as Breakpoints
import qualified HGBC.Debugger.Disassembly     as Disassembly
import qualified HGBC.Debugger.Events          as Event
import qualified HGBC.Debugger.JSON            as JSON
import qualified HGBC.Debugger.Labels          as Labels
import qualified HGBC.Debugger.Memory          as Memory
import qualified HGBC.Debugger.Resources       as Resource
import qualified HGBC.Emulator
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp

start
  :: FilePath
  -> HGBC.Config.Config k Identity
  -> HGBC.Emulator.Emulator
  -> EmulatorState
  -> DebugState
  -> IO Event.Channel
start romFileName HGBC.Config.Config {..} emulator emulatorState debugState = do
  channel <- Event.newChannel
  void $ forkIO $ Warp.run debugPort
                           (debugger channel romFileName emulator emulatorState debugState)
  pure channel

debugger
  :: Event.Channel
  -> FilePath
  -> HGBC.Emulator.Emulator
  -> EmulatorState
  -> DebugState
  -> Wai.Application
debugger channel romFileName emulator emulatorState debugState req respond =
  respond =<< case Wai.pathInfo req of
    [] -> case Wai.requestMethod req of
      "GET" -> pure
        (Wai.responseLBS HTTP.status200
                         [(HTTP.hContentType, "text/html")]
                         (debugHTML romFileName (bootROMLength (memory emulatorState)))
        )
      "POST" -> do
        body <- Wai.lazyRequestBody req
        case HTTP.parseQuery (LBC.toStrict body) of
          [("run", _)] ->
            emptyResponse <$ HGBC.Emulator.sendNotification emulator HGBC.Emulator.PauseNotification
          [("step", _)] ->
            emptyResponse <$ HGBC.Emulator.sendNotification emulator HGBC.Emulator.StepNotification
          [("stepOver", _)] ->
            emptyResponse
              <$ HGBC.Emulator.sendNotification emulator HGBC.Emulator.StepOverNotification
          [("stepOut", _)] ->
            emptyResponse
              <$ HGBC.Emulator.sendNotification emulator HGBC.Emulator.StepOutNotification
          [("restart", _)] ->
            emptyResponse
              <$ HGBC.Emulator.sendNotification emulator HGBC.Emulator.RestartNotification
          [("runTo", _), ("bank", Just bank), ("offset", Just offset)] ->
            case LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset of
              Nothing      -> invalidCommandParamters "runTo"
              Just address -> emptyResponse <$ HGBC.Emulator.sendNotification
                emulator
                (HGBC.Emulator.RunToNotification address)
          _ -> invalidCommand ""
      _ -> badMethod

    ["css"]                        -> cssResponse Resource.css
    ["js" ]                        -> jsResponse Resource.js
    ["svg", "run"                ] -> svgResponse Resource.svgRun
    ["svg", "pause"              ] -> svgResponse Resource.svgPause
    ["svg", "step"               ] -> svgResponse Resource.svgStep
    ["svg", "stepout"            ] -> svgResponse Resource.svgStepout
    ["svg", "stepthrough"        ] -> svgResponse Resource.svgStepthrough
    ["svg", "reset"              ] -> svgResponse Resource.svgReset
    ["svg", "runto"              ] -> svgResponse Resource.svgRunto
    ["svg", "breakpoint"         ] -> svgResponse Resource.svgBreakpoint
    ["svg", "breakpoint_disabled"] -> svgResponse Resource.svgBreakpointDisabled
    ["svg", "home"               ] -> svgResponse Resource.svgHome
    ["svg", "label"              ] -> svgResponse Resource.svgLabel
    ["svg", "download"           ] -> svgResponse Resource.svgDownload

    ["memory"]                     -> whenMethodGET $ case Wai.queryString req of
      [("address", Just addressText), ("lines", Just rawLines)] ->
        case (,) <$> readMaybe ("0x" <> BC.unpack addressText) <*> readMaybe (BC.unpack rawLines) of
          Nothing -> invalidQuery
          Just (address, memLines) ->
            textResponse <$> Memory.memoryAt emulatorState address memLines
      _ -> invalidQuery

    ["backtrace"] -> whenMethodGET $ jsonResponse <$> Memory.backtrace emulatorState

    ["stack"    ] -> whenMethodGET $ case Wai.queryString req of
      [("offset", Just offsetText), ("n", Just linesText)] ->
        case (,) <$> readMaybeHexText offsetText <*> readMaybeText linesText of
          Nothing          -> invalidQuery
          Just (offset, n) -> textResponse <$> Memory.stackAt emulatorState offset n

      _ -> invalidQuery

    ["disassembly"] -> whenMethodGET $ case Wai.queryString req of
      [] -> do
        disassembly <- Disassembly.getAll debugState
        pure
          (Wai.responseLBS
            HTTP.status200
            [ (HTTP.hContentType    , "text/plain; charset=UTF-8")
            , ("Content-Disposition", "attachment")
            , (HTTP.hCacheControl   , "no-cache")
            ]
            (LT.encodeUtf8 disassembly)
          )

      [("bank", Just bank), ("offset", Just offset), ("n", Just linesText)] ->
        case
            (,)
            <$> (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
            <*> readMaybeText linesText
          of
            Nothing           -> invalidQuery
            Just (address, n) -> jsonResponse <$> Disassembly.getN debugState address n
      _ -> invalidQuery

    ["breakpoints"] -> case Wai.requestMethod req of
      "GET" ->
        jsonResponse . JSON.toLazyByteString . JSON.breakpoints <$> Breakpoints.getAsList debugState

      "POST" -> withAddress $ \address -> do
        body <- Wai.lazyRequestBody req
        case HTTP.parseQuery (LBC.toStrict body) of
          [("set"    , _)] -> emptyResponse <$ Breakpoints.set debugState channel address
          [("disable", _)] -> emptyResponse <$ Breakpoints.disable debugState channel address
          [("unset"  , _)] -> emptyResponse <$ Breakpoints.unset debugState channel address
          _                -> invalidCommand "breakpoints"
      _ -> badMethod

    ["labels"] -> whenMethodGET
      (jsonResponse . JSON.toLazyByteString . JSON.labels <$> Labels.getAsList debugState)

    ["label"] -> withAddress $ \address -> whenMethodPOST $ do
      body <- Wai.lazyRequestBody req
      case HTTP.parseQuery (LBC.toStrict body) of
        [("update", Just rawText)] -> emptyResponse
          <$ Labels.update debugState channel address (T.decodeUtf8With T.lenientDecode rawText)
        [("delete", _)] -> emptyResponse <$ Labels.delete debugState channel address
        _               -> invalidCommand "label"

    ["events"] -> whenMethodGET
      (pure
        (Wai.responseStream
          HTTP.status200
          [(HTTP.hContentType, "text/event-stream"), (HTTP.hCacheControl, "no-cache")]
          (Event.stream channel emulatorState)
        )
      )
    _ -> resourceNotFound

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
  readMaybeHexText t = readMaybe ("0x" <> BC.unpack t)
  readMaybeText t = readMaybe (BC.unpack t)
  whenMethodGET handler = if Wai.requestMethod req == "GET" then handler else badMethod
  whenMethodPOST handler = if Wai.requestMethod req == "POST" then handler else badMethod
  withAddress handler = case Wai.queryString req of
    [("bank", Just bank), ("offset", Just offset)] ->
      maybe invalidQuery handler (LongAddress <$> readMaybeHexText bank <*> readMaybeHexText offset)
    _ -> invalidQuery

  textResponse = Wai.responseLBS
    HTTP.status200
    [(HTTP.hContentType, "text/plain"), (HTTP.hCacheControl, "no-cache")]

  jsonResponse = Wai.responseLBS
    HTTP.status200
    [(HTTP.hContentType, "application/json"), (HTTP.hCacheControl, "no-cache")]

  svgResponse content = whenMethodGET
    (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "image/svg+xml")] content))
  cssResponse content =
    whenMethodGET (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/css")] content))
  jsResponse content = whenMethodGET
    (pure (Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/javascript")] content))
