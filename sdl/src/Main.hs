{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Machine.GBC
import           Machine.GBC.CPU                ( readPC )
import           Machine.GBC.Disassembler
import           Machine.GBC.Memory             ( getBank )
import           Machine.GBC.ROM
import           Machine.GBC.Util
import           System.Directory
import           System.FilePath
import qualified Audio
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.HashMap.Lazy             as HM
import qualified Data.HashTable.IO             as H
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified HGBC.Debugger.Labels          as Labels
import qualified HGBC.Debugger.Events          as Event
import qualified HGBC.Config.Paths             as Path
import qualified HGBC.Config                   as Config
import qualified HGBC.Config.CommandLine       as CommandLine
import qualified HGBC.Debugger                 as Debugger
import qualified HGBC.Emulator                 as Emulator
import qualified HGBC.Keymap                   as Keymap
import qualified Machine.GBC.Keypad            as GBC
import qualified SDL
import qualified Thread.EventLoop              as EventLoop
import qualified Thread.LCD                    as LCD
import qualified Window

readBootROMFile :: Maybe FilePath -> IO (Maybe B.ByteString)
readBootROMFile Nothing            = pure Nothing
readBootROMFile (Just bootROMFile) = do
  mContent <- try (B.readFile bootROMFile)
  case mContent of
    Left err -> do
      putStrLn
        ("Cannot load boot ROM file " <> bootROMFile <> ":\n" <> displayException
          (err :: IOException)
        )
      pure Nothing
    Right content -> pure (Just content)

main :: IO ()
main = do
  SDL.initializeAll
  (errors, options, romPaths, config) <- Config.configure decodeScancode defaultKeymap
  fileContent                         <- B.readFile (CommandLine.filename options)
  let (eROM, warnings) = runWriter (runExceptT (parseROM romPaths fileContent))

  unless (null errors) $ for_ errors $ \(fileName, fileErrors) -> do
    putStrLn ("Errors in " <> fileName)
    for_ fileErrors $ \err -> putStrLn ("  " <> err)

  unless (null warnings) $ do
    putStrLn ("Some problems were detected in ROM file " <> CommandLine.filename options <> ":")
    for_ warnings $ \message -> putStrLn (" - " <> message)

  case eROM of
    Left err -> do
      putStrLn ("Cannot read ROM file " <> CommandLine.filename options <> " because:")
      putStrLn (" - " <> err)
    Right rom -> do
      when (requiresSaveFiles rom)
        $ createDirectoryIfMissing True (takeDirectory (romSaveFile romPaths))
      emulator rom options config

emulator :: ROM -> CommandLine.Options -> Config.Config SDL.Scancode Identity -> IO ()
emulator rom options config = do
  dumpROMHeader (romHeader rom)
  graphicsSync          <- newGraphicsSync
  emulatorChannel       <- Emulator.new
  (window, frameBuffer) <- LCD.start (takeBaseName (CommandLine.filename options))
                                     config
                                     graphicsSync
  bootROM       <- readBootROMFile (Config.bootROM config)
  emulatorState <- initEmulatorState bootROM
                                     rom
                                     (Config.mode config)
                                     (Config.colorCorrection config)
                                     graphicsSync
                                     frameBuffer

  when (mode emulatorState == DMG) $ do
    writeBgRGBPalette emulatorState 0 (Config.backgroundPalette config)
    writeFgRGBPalette emulatorState 0 (Config.sprite1Palette config)
    writeFgRGBPalette emulatorState 1 (Config.sprite2Palette config)

  audio <- if CommandLine.noSound options then pure Nothing else Just <$> Audio.start emulatorState
  EventLoop.start window (Config.keypad config) emulatorChannel emulatorState

  let romFileName = takeBaseName (CommandLine.filename options)
  disassemblyRef   <- newIORef mempty
  breakpoints      <- H.new
  labelsRef        <- newIORef (HM.fromList initialLabels)
  bootDebuggerPath <- traverse Path.debugState (Config.bootROM config)
  romDebuggerPath  <- Path.debugState romFileName
  let debugState = Debugger.DebugState { .. }

  debuggerChannel <- if CommandLine.debugMode options
    then Just <$> Debugger.start romFileName config emulatorChannel emulatorState debugState
    else pure Nothing

  case debuggerChannel of
    Nothing      -> pure ()
    Just channel -> do
      Debugger.restoreBreakpoints debugState
      Debugger.restoreLabels debugState
      restoredLabels        <- readIORef labelsRef
      (disassembly, labels) <- disassembleROM (memory emulatorState) (HM.keys restoredLabels)
      writeIORef disassemblyRef disassembly
      Labels.addFromList debugState channel labels

  let pauseAudio  = maybe (pure ()) Audio.pause audio
  let resumeAudio = maybe (pure ()) Audio.resume audio

  let notifyPaused = do
        Window.sendNotification window Window.PausedNotification
        maybe (pure ()) (`Event.send` Event.EmulatorPaused) debuggerChannel

  let notifyResumed = do
        Window.sendNotification window Window.ResumedNotification
        maybe (pure ()) (`Event.send` Event.EmulatorStarted) debuggerChannel

  let onQuit =
        -- Switch off audio so that the audio callback is not invoked
        -- after the Haskell RTS has shut down.
        pauseAudio

  let
    emulatorLoop runToAddress level = do
      emulatorClock <- getEmulatorClock
      now           <- liftIO SDL.time
      let
        innerEmulatorLoop !steps = do
          step
          breakRequired <- if CommandLine.debugMode options
            then do
              address <- getCurrentAddress
              updateDisassembly address
              breakpoint <- liftIO $ H.lookup breakpoints address
              callDepth  <- getCPUCallDepth
              pure
                (Just address == runToAddress || breakpoint == Just True || Just callDepth == level)
            else pure True

          if breakRequired
            then pauseAudio >> pauseLoop
            else if steps .&. 0x1FFFFF == 0
              then do
                when (CommandLine.stats options) $ printPerformanceStats emulatorClock now
                emulatorLoop runToAddress level
              else if steps .&. 0xFFFF == 0
                then do
                  notification <- Emulator.getNotification emulatorChannel
                  case notification of
                    Just Emulator.PauseNotification -> pauseAudio >> pauseLoop
                    Just Emulator.QuitNotification -> onQuit
                    Just Emulator.RestartNotification -> reset >> innerEmulatorLoop (steps + 1)
                    _ -> innerEmulatorLoop (steps + 1)
                else innerEmulatorLoop (steps + 1)
      innerEmulatorLoop (1 :: Int)

    pauseLoop = do
      notifyPaused
      notification <- Emulator.waitNotification emulatorChannel
      case notification of
        Emulator.QuitNotification  -> onQuit
        Emulator.PauseNotification -> resumeAudio >> notifyResumed >> emulatorLoop Nothing Nothing
        Emulator.RunNotification   -> resumeAudio >> notifyResumed >> emulatorLoop Nothing Nothing
        Emulator.RunToNotification address ->
          resumeAudio >> notifyResumed >> emulatorLoop (Just address) Nothing
        Emulator.StepNotification     -> singleStep >> pauseLoop
        Emulator.StepOverNotification -> do
          callDepth0 <- getCPUCallDepth
          notifyResumed >> singleStep
          callDepth1 <- getCPUCallDepth
          if callDepth1 > callDepth0 then emulatorLoop Nothing (Just callDepth0) else pauseLoop
        Emulator.StepOutNotification -> do
          callDepth <- getCPUCallDepth
          notifyResumed
          emulatorLoop Nothing (Just (callDepth - 1))
        Emulator.RestartNotification -> reset >> pauseLoop

    singleStep = step >> getCurrentAddress >>= updateDisassembly

    updateDisassembly address@(LongAddress _ pc) = do
      disassembly <- liftIO (readIORef disassemblyRef)
      r           <- disassemblyRequired address disassembly
      when r $ do
        (disassembly', newLabels) <- disassembleFrom pc disassembly
        case debuggerChannel of
          Nothing      -> pure ()
          Just channel -> liftIO $ do
            writeIORef disassemblyRef disassembly'
            Labels.addFromList debugState channel newLabels

  runReaderT
    (reset >> if CommandLine.debugMode options
      then pauseLoop
      else resumeAudio >> emulatorLoop Nothing Nothing
    )
    emulatorState

 where
  getCurrentAddress :: ReaderT EmulatorState IO LongAddress
  getCurrentAddress = do
    pc   <- readPC
    bank <- getBank pc
    pure (LongAddress bank pc)

  printPerformanceStats :: Int -> Double -> ReaderT EmulatorState IO ()
  printPerformanceStats emulatorClock now = do
    now'           <- liftIO SDL.time
    emulatorClock' <- getEmulatorClock
    let clocks          = emulatorClock' - emulatorClock
    let time            = now' - now
    let percentageSpeed = (fromIntegral clocks / (time * 4 * 1024 * 1024)) * 100
    liftIO $ putStrLn
      (  take 5 (show percentageSpeed)
      <> "% ("
      <> show clocks
      <> " clocks in "
      <> show time
      <> " seconds)"
      )

  dumpROMHeader :: Header -> IO ()
  dumpROMHeader Header {..} = do
    putStrLn
      $  take 11 (BC.unpack gameTitle ++ repeat ' ')
      ++ BC.unpack gameCode
      ++ " "
      ++ case destination of
           Japan    -> " (JAPAN)"
           Overseas -> " (INTERNATIONAL)"

    putStrLn ("Version: " ++ show maskROMVersion)
    putStrLn ("Maker: " ++ formatHex oldLicenseCode ++ " " ++ BC.unpack makerCode)

    putStr $ "Console support: " ++ case cgbSupport of
      CGBIncompatible -> "GB"
      CGBCompatible   -> "GB+CGB"
      CGBExclusive    -> "CGB"
    putStrLn $ case sgbSupport of
      GBOnly  -> ""
      UsesSGB -> "+SGB"

    let cartridge = "Cartridge: " ++ case mbcType cartridgeType of
          Nothing      -> "No MBC"
          Just MBC1    -> "MBC1"
          Just MBC2    -> "MBC2"
          Just MBC3    -> "MBC3"
          Just MBC3RTC -> "MBC3+RTC"
          Just MBC5    -> "MBC5"
    putStrLn
      (  cartridge
      ++ (if hasSRAM cartridgeType then "+SRAM" else "")
      ++ (if hasBackupBattery cartridgeType then "+Battery" else "")
      ++ " ("
      ++ formatByteCount romSize
      ++ " ROM"
      ++ (if externalRAM > 0 then " + " ++ formatByteCount externalRAM ++ " RAM)" else ")")
      )

    putStrLn ("Start address: " ++ formatHex startAddress)
    putStrLn ""

  formatByteCount :: Int -> String
  formatByteCount b | b < 1024        = show b
                    | b < 1024 * 1024 = show (b `div` 1024) ++ "KiB"
                    | otherwise       = show (b `div` (1024 * 1024)) ++ "MiB"


-- | The default keymap.
defaultKeymap :: Keymap.Keymap SDL.Scancode
defaultKeymap = Keymap.Keymap $ M.fromList
  [ ((SDL.ScancodeZ, [])              , Keymap.GBCKey GBC.KeyA)
  , ((SDL.ScancodeX, [])              , Keymap.GBCKey GBC.KeyB)
  , ((SDL.ScancodeReturn, [])         , Keymap.GBCKey GBC.KeyStart)
  , ((SDL.ScancodeBackspace, [])      , Keymap.GBCKey GBC.KeySelect)
  , ((SDL.ScancodeUp, [])             , Keymap.GBCKey GBC.KeyUp)
  , ((SDL.ScancodeDown, [])           , Keymap.GBCKey GBC.KeyDown)
  , ((SDL.ScancodeLeft, [])           , Keymap.GBCKey GBC.KeyLeft)
  , ((SDL.ScancodeRight, [])          , Keymap.GBCKey GBC.KeyRight)
  , ((SDL.ScancodePause, [])          , Keymap.Pause)
  , ((SDL.ScancodeK, [])              , Keymap.Pause)
  , ((SDL.ScancodeQ, [Keymap.Control]), Keymap.Quit)
  ]

decodeScancode :: T.Text -> Maybe (SDL.Scancode, [Keymap.Modifier])
decodeScancode "" = Nothing
decodeScancode text =
  (attachModifiers SDL.ScancodeKPMinus =<< T.stripSuffix "KP-" text)
    <|> (attachModifiers SDL.ScancodeMinus =<< T.stripSuffix "-" text)
    <|> (case reverse (T.splitOn "-" (T.toUpper text)) of
          []                -> Nothing
          (key : modifiers) -> (,) <$> decodeMain key <*> traverse decodeModifier modifiers
        )

 where
  attachModifiers key modifiers = (key, ) <$> decodeModifiers modifiers
  decodeModifiers "" = Just []
  decodeModifiers modifiers =
    traverse decodeModifier . T.splitOn "-" =<< T.stripSuffix "-" modifiers

  decodeModifier "C" = Just Keymap.Control
  decodeModifier "A" = Just Keymap.Alt
  decodeModifier "S" = Just Keymap.Shift
  decodeModifier _   = Nothing

  decodeMain "`"           = Just SDL.ScancodeGrave
  decodeMain "1"           = Just SDL.Scancode1
  decodeMain "2"           = Just SDL.Scancode2
  decodeMain "3"           = Just SDL.Scancode3
  decodeMain "4"           = Just SDL.Scancode4
  decodeMain "5"           = Just SDL.Scancode5
  decodeMain "6"           = Just SDL.Scancode6
  decodeMain "7"           = Just SDL.Scancode7
  decodeMain "8"           = Just SDL.Scancode8
  decodeMain "9"           = Just SDL.Scancode9
  decodeMain "0"           = Just SDL.Scancode0
  decodeMain "="           = Just SDL.ScancodeEquals
  decodeMain "Q"           = Just SDL.ScancodeQ
  decodeMain "W"           = Just SDL.ScancodeW
  decodeMain "E"           = Just SDL.ScancodeE
  decodeMain "R"           = Just SDL.ScancodeR
  decodeMain "T"           = Just SDL.ScancodeT
  decodeMain "Y"           = Just SDL.ScancodeY
  decodeMain "U"           = Just SDL.ScancodeU
  decodeMain "I"           = Just SDL.ScancodeI
  decodeMain "O"           = Just SDL.ScancodeO
  decodeMain "P"           = Just SDL.ScancodeP
  decodeMain "["           = Just SDL.ScancodeLeftBracket
  decodeMain "]"           = Just SDL.ScancodeRightBracket
  decodeMain "\\"          = Just SDL.ScancodeBackslash
  decodeMain "A"           = Just SDL.ScancodeA
  decodeMain "S"           = Just SDL.ScancodeS
  decodeMain "D"           = Just SDL.ScancodeD
  decodeMain "F"           = Just SDL.ScancodeF
  decodeMain "G"           = Just SDL.ScancodeG
  decodeMain "H"           = Just SDL.ScancodeH
  decodeMain "J"           = Just SDL.ScancodeJ
  decodeMain "K"           = Just SDL.ScancodeK
  decodeMain "L"           = Just SDL.ScancodeL
  decodeMain ";"           = Just SDL.ScancodeSemicolon
  decodeMain "'"           = Just SDL.ScancodeApostrophe
  decodeMain "Z"           = Just SDL.ScancodeZ
  decodeMain "X"           = Just SDL.ScancodeX
  decodeMain "C"           = Just SDL.ScancodeC
  decodeMain "V"           = Just SDL.ScancodeV
  decodeMain "B"           = Just SDL.ScancodeB
  decodeMain "N"           = Just SDL.ScancodeN
  decodeMain "M"           = Just SDL.ScancodeM
  decodeMain ","           = Just SDL.ScancodeComma
  decodeMain "."           = Just SDL.ScancodePeriod
  decodeMain "/"           = Just SDL.ScancodeSlash
  decodeMain "F1"          = Just SDL.ScancodeF1
  decodeMain "F2"          = Just SDL.ScancodeF2
  decodeMain "F3"          = Just SDL.ScancodeF3
  decodeMain "F4"          = Just SDL.ScancodeF4
  decodeMain "F5"          = Just SDL.ScancodeF5
  decodeMain "F6"          = Just SDL.ScancodeF6
  decodeMain "F7"          = Just SDL.ScancodeF7
  decodeMain "F8"          = Just SDL.ScancodeF8
  decodeMain "F9"          = Just SDL.ScancodeF9
  decodeMain "F10"         = Just SDL.ScancodeF10
  decodeMain "F11"         = Just SDL.ScancodeF11
  decodeMain "F12"         = Just SDL.ScancodeF12
  decodeMain "F13"         = Just SDL.ScancodeF13
  decodeMain "F14"         = Just SDL.ScancodeF14
  decodeMain "F15"         = Just SDL.ScancodeF15
  decodeMain "F16"         = Just SDL.ScancodeF16
  decodeMain "F17"         = Just SDL.ScancodeF17
  decodeMain "F18"         = Just SDL.ScancodeF18
  decodeMain "F19"         = Just SDL.ScancodeF19
  decodeMain "F20"         = Just SDL.ScancodeF20
  decodeMain "F21"         = Just SDL.ScancodeF21
  decodeMain "F22"         = Just SDL.ScancodeF22
  decodeMain "F23"         = Just SDL.ScancodeF23
  decodeMain "F24"         = Just SDL.ScancodeF24
  decodeMain "ESC"         = Just SDL.ScancodeEscape
  decodeMain "TAB"         = Just SDL.ScancodeTab
  decodeMain "LSHIFT"      = Just SDL.ScancodeLShift
  decodeMain "LCONTROL"    = Just SDL.ScancodeLCtrl
  decodeMain "LALT"        = Just SDL.ScancodeLAlt
  decodeMain "BACKSPACE"   = Just SDL.ScancodeBackspace
  decodeMain "ENTER"       = Just SDL.ScancodeReturn
  decodeMain "RSHIFT"      = Just SDL.ScancodeRShift
  decodeMain "RALT"        = Just SDL.ScancodeRAlt
  decodeMain "RCONTROL"    = Just SDL.ScancodeRCtrl
  decodeMain "INSERT"      = Just SDL.ScancodeInsert
  decodeMain "DELETE"      = Just SDL.ScancodeDelete
  decodeMain "HOME"        = Just SDL.ScancodeHome
  decodeMain "END"         = Just SDL.ScancodeEnd
  decodeMain "PAGEUP"      = Just SDL.ScancodePageUp
  decodeMain "PAGEDOWN"    = Just SDL.ScancodePageDown
  decodeMain "UP"          = Just SDL.ScancodeUp
  decodeMain "DOWN"        = Just SDL.ScancodeDown
  decodeMain "LEFT"        = Just SDL.ScancodeLeft
  decodeMain "RIGHT"       = Just SDL.ScancodeRight
  decodeMain "KP0"         = Just SDL.ScancodeKP0
  decodeMain "KP1"         = Just SDL.ScancodeKP1
  decodeMain "KP2"         = Just SDL.ScancodeKP2
  decodeMain "KP3"         = Just SDL.ScancodeKP3
  decodeMain "KP4"         = Just SDL.ScancodeKP4
  decodeMain "KP5"         = Just SDL.ScancodeKP5
  decodeMain "KP6"         = Just SDL.ScancodeKP6
  decodeMain "KP7"         = Just SDL.ScancodeKP7
  decodeMain "KP8"         = Just SDL.ScancodeKP8
  decodeMain "KP9"         = Just SDL.ScancodeKP9
  decodeMain "KP/"         = Just SDL.ScancodeKPDivide
  decodeMain "KP*"         = Just SDL.ScancodeKPMultiply
  decodeMain "KP+"         = Just SDL.ScancodeKPPlus
  decodeMain "KPENTER"     = Just SDL.ScancodeKPEnter
  decodeMain "KP."         = Just SDL.ScancodeKPPeriod
  decodeMain "PAUSE"       = Just SDL.ScancodePause
  decodeMain "PRINTSCREEN" = Just SDL.ScancodePrintScreen
  decodeMain _             = Nothing
