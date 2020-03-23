{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.FileEmbed
import           Data.Foldable
import           Data.Functor
import           Machine.GBC
import           Options.Applicative
import           System.Directory
import           System.FilePath
import qualified Audio
import qualified Config
import qualified Data.ByteString               as B
import qualified Emulator
import qualified SDL
import qualified Thread.EventLoop              as EventLoop
import qualified Thread.LCD                    as LCD
import qualified Window

data Options = Options
  { debugMode   :: Bool
  , noSound     :: Bool
  , scaleFactor :: Maybe Int
  , speedFactor :: Maybe Double
  , filename    :: FilePath
  }

optionsToConfig :: Options -> Config.Config Maybe
optionsToConfig Options {..} = mempty { Config.scale = scaleFactor, Config.speed = speedFactor }

optionsP :: Parser Options
optionsP =
  Options
    <$> switch (long "debug" <> help "Enable the debugger")
    <*> switch (long "no-sound" <> help "Disable audio output")
    <*> option
          (Just <$> auto)
          (long "scale" <> value Nothing <> metavar "SCALE" <> help
            "Default scale factor for video output"
          )
    <*> option
          (Just <$> auto)
          (long "speed" <> value Nothing <> metavar "SPEED" <> help
            "Speed as a fraction of normal speed"
          )
    <*> strArgument (metavar "ROM-FILE" <> help "The ROM file to run")

description :: ParserInfo Options
description =
  info (optionsP <**> helper) (fullDesc <> header "hgbc-sdl - a Gameboy Color emulator")

hgbcBaseDir :: IO FilePath
hgbcBaseDir = getAppUserDataDirectory "hgbc"

getEffectiveConfig :: ROM -> Options -> IO (Config.Config Identity)
getEffectiveConfig rom options = do
  mainConfig <- getMainConfig
  romConfig  <- getROMConfig (romPaths rom)
  pure (Config.finalize (mainConfig <> romConfig <> optionsToConfig options))

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

main :: IO ()
main = do
  SDL.initializeAll
  allOptions@Options {..} <- execParser description
  fileContent             <- B.readFile filename
  romPaths                <- getROMPaths filename
  let (eROM, warnings) = runWriter (runExceptT (parseROM romPaths fileContent))

  unless (null warnings) $ do
    putStrLn ("Some problems were detected in " <> filename <> ":")
    for_ warnings $ \message -> putStrLn (" - " <> message)

  case eROM of
    Left err -> do
      putStrLn ("Cannot load " <> filename <> " because:")
      putStrLn (" - " <> err)
    Right rom -> do
      when (requiresSaveFiles rom)
        $ createDirectoryIfMissing True (takeDirectory (romSaveFile romPaths))
      emulator rom allOptions

emulator :: ROM -> Options -> IO ()
emulator rom allOptions@Options {..} = do
  config                <- getEffectiveConfig rom allOptions
  graphicsSync          <- newGraphicsSync
  emulatorChannel       <- Emulator.new
  (window, frameBuffer) <- LCD.start (takeBaseName filename) config graphicsSync
  emulatorState         <- initEmulatorState rom graphicsSync frameBuffer

  when (mode emulatorState == DMG) $ do
    writeBgRGBPalette emulatorState 0 (Config.backgroundPalette config)
    writeFgRGBPalette emulatorState 0 (Config.sprite1Palette config)
    writeFgRGBPalette emulatorState 1 (Config.sprite2Palette config)

  audio <- if noSound then pure Nothing else Just <$> Audio.start emulatorState
  EventLoop.start window (Config.keypad config) emulatorChannel emulatorState

  maybe (pure ()) Audio.resume audio

  let onQuit =
        -- Switch off audio so that the audio callback is not invoked
        -- after the Haskell RTS has shut down.
        maybe (pure ()) Audio.pause audio

  let emulatorLoop = do
        step
        notification <- Emulator.getNotification emulatorChannel
        case notification of
          Nothing                         -> emulatorLoop
          Just Emulator.QuitNotification  -> onQuit
          Just Emulator.PauseNotification -> do
            maybe (pure ()) Audio.pause audio
            Window.sendNotification window Window.PausedNotification
            notification1 <- Emulator.waitNotification emulatorChannel
            case notification1 of
              Emulator.PauseNotification -> do
                maybe (pure ()) Audio.resume audio
                Window.sendNotification window Window.ResumedNotification
                emulatorLoop
              Emulator.QuitNotification -> onQuit

  runReaderT (reset >> emulatorLoop) emulatorState
