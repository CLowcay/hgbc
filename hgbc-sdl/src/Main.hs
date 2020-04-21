{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  )
where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Bits
import           Data.FileEmbed
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Disassembler
import           Machine.GBC
import           Machine.GBC.CPU                ( readPC
                                                , getCPUCallDepth
                                                )
import           Machine.GBC.Memory             ( getBank )
import           Machine.GBC.ROM
import           Machine.GBC.Util
import           Options.Applicative            ( (<**>) )
import           System.Directory
import           System.FilePath
import qualified Audio
import qualified Config
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.HashTable.IO             as H
import qualified Debugger
import qualified Emulator
import qualified Options.Applicative           as Opt
import qualified SDL
import qualified Thread.EventLoop              as EventLoop
import qualified Thread.LCD                    as LCD
import qualified Window

description :: Opt.ParserInfo Config.Options
description = Opt.info (Config.optionsP <**> Opt.helper)
                       (Opt.fullDesc <> Opt.header "hgbc-sdl - a Gameboy Color emulator")

hgbcBaseDir :: IO FilePath
hgbcBaseDir = getAppUserDataDirectory "hgbc"

getEffectiveConfig :: ROM -> Config.Options -> IO (Config.Config Identity)
getEffectiveConfig rom options = do
  mainConfig <- getMainConfig
  romConfig  <- getROMConfig (romPaths rom)
  pure (Config.finalize (mainConfig <> romConfig <> Config.optionsToConfig options))

getMainConfig :: IO (Config.Config Maybe)
getMainConfig = do
  baseDir <- hgbcBaseDir
  let configFile = baseDir </> "config.toml"
  exists <- doesFileExist configFile
  unless exists $ do
    createDirectoryIfMissing True baseDir
    B.writeFile configFile $(embedOneFileOf ["data/default.toml", "../data/default.toml"])

  handleConfigErrors configFile =<< Config.parseConfigFile configFile

getROMConfig :: ROMPaths -> IO (Config.Config Maybe)
getROMConfig ROMPaths {..} = do
  let configFile = takeDirectory romSaveFile </> "config.toml"
  exists <- doesFileExist configFile
  if exists
    then handleConfigErrors configFile =<< Config.parseConfigFile configFile
    else pure mempty

handleConfigErrors :: FilePath -> Either [String] (Config.Config Maybe) -> IO (Config.Config Maybe)
handleConfigErrors _          (Right config) = pure config
handleConfigErrors configFile (Left  errors) = do
  putStrLn ("Cannot parse " <> configFile <> ":")
  for_ errors putStrLn
  pure mempty

getROMPaths :: FilePath -> IO ROMPaths
getROMPaths romFile = do
  baseDir <- hgbcBaseDir <&> (</> "rom")
  let romDir      = baseDir </> takeBaseName romFile
  let romSaveFile = romDir </> "battery"
  let romRTCFile  = romDir </> "rtc"
  pure ROMPaths { .. }

getLabelsPath :: FilePath -> IO FilePath
getLabelsPath romFile = hgbcBaseDir <&> (</> "rom" </> takeBaseName romFile)

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
  allOptions@Config.Options {..} <- Opt.execParser description
  fileContent                    <- B.readFile optionFilename
  romPaths                       <- getROMPaths optionFilename
  let (eROM, warnings) = runWriter (runExceptT (parseROM romPaths fileContent))

  unless (null warnings) $ do
    putStrLn ("Some problems were detected in ROM file " <> optionFilename <> ":")
    for_ warnings $ \message -> putStrLn (" - " <> message)

  case eROM of
    Left err -> do
      putStrLn ("Cannot read ROM file " <> optionFilename <> " because:")
      putStrLn (" - " <> err)
    Right rom -> do
      when (requiresSaveFiles rom)
        $ createDirectoryIfMissing True (takeDirectory (romSaveFile romPaths))
      emulator rom allOptions

emulator :: ROM -> Config.Options -> IO ()
emulator rom allOptions@Config.Options {..} = do
  dumpROMHeader (romHeader rom)
  config                <- getEffectiveConfig rom allOptions
  graphicsSync          <- newGraphicsSync
  emulatorChannel       <- Emulator.new
  (window, frameBuffer) <- LCD.start (takeBaseName optionFilename) config graphicsSync
  bootROM               <- readBootROMFile (Config.bootROM config)
  emulatorState         <- initEmulatorState bootROM
                                             rom
                                             (Config.mode config)
                                             (Config.colorCorrection config)
                                             graphicsSync
                                             frameBuffer

  when (mode emulatorState == DMG) $ do
    writeBgRGBPalette emulatorState 0 (Config.backgroundPalette config)
    writeFgRGBPalette emulatorState 0 (Config.sprite1Palette config)
    writeFgRGBPalette emulatorState 1 (Config.sprite2Palette config)

  audio <- if optionNoSound then pure Nothing else Just <$> Audio.start emulatorState
  EventLoop.start window (Config.keypad config) emulatorChannel emulatorState

  let romFileName = takeBaseName optionFilename
  disassemblyRef   <- newIORef mempty
  breakpoints      <- H.new
  labelsRef        <- newIORef mempty
  bootDebuggerPath <- traverse getLabelsPath (Config.bootROM config)
  romDebuggerPath  <- getLabelsPath romFileName
  let debugState = Debugger.DebugState { .. }

  debuggerChannel <- if optionDebugMode
    then Just <$> Debugger.start romFileName config emulatorChannel emulatorState debugState
    else pure Nothing

  case debuggerChannel of
    Nothing      -> pure ()
    Just channel -> do
      Debugger.restoreLabels debugState
      (disassembly, labels) <- disassembleROM (memory emulatorState)
      writeIORef disassemblyRef disassembly
      Debugger.addNewLabels debugState channel labels

  let pauseAudio  = maybe (pure ()) Audio.pause audio
  let resumeAudio = maybe (pure ()) Audio.resume audio

  let notifyPaused = do
        Window.sendNotification window Window.PausedNotification
        maybe (pure ()) (`Debugger.sendNotification` Debugger.EmulatorPaused) debuggerChannel

  let notifyResumed = do
        Window.sendNotification window Window.ResumedNotification
        maybe (pure ()) (`Debugger.sendNotification` Debugger.EmulatorStarted) debuggerChannel

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
          breakRequired <- if optionDebugMode
            then do
              pc   <- readPC
              bank <- getBank pc
              let address = LongAddress bank pc
              disassembly <- liftIO (readIORef disassemblyRef)
              r           <- disassemblyRequired address disassembly
              when r $ do
                (disassembly', newLabels) <- disassembleFrom pc disassembly
                case debuggerChannel of
                  Nothing      -> pure ()
                  Just channel -> liftIO $ do
                    writeIORef disassemblyRef disassembly'
                    Debugger.addNewLabels debugState channel newLabels
              breakpoint <- liftIO $ H.lookup breakpoints address
              callDepth  <- getCPUCallDepth
              pure (Just address == runToAddress || isJust breakpoint || Just callDepth == level)
            else pure True

          if breakRequired
            then pauseAudio >> pauseLoop
            else if steps .&. 0x1FFFFF == 0
              then do
                when optionStats $ printPerformanceStats emulatorClock now
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
        Emulator.StepNotification     -> step >> pauseLoop
        Emulator.StepOverNotification -> do
          callDepth0 <- getCPUCallDepth
          step
          callDepth1 <- getCPUCallDepth
          if callDepth1 > callDepth0 then emulatorLoop Nothing (Just callDepth0) else pauseLoop
        Emulator.StepOutNotification -> do
          callDepth <- getCPUCallDepth
          emulatorLoop Nothing (Just (callDepth - 1))
        Emulator.RestartNotification -> reset >> pauseLoop

  runReaderT
    (reset >> if optionDebugMode then pauseLoop else resumeAudio >> emulatorLoop Nothing Nothing)
    emulatorState

 where
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
