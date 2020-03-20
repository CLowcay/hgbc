{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Foldable
import           Data.Functor
import           Keymap
import           Machine.GBC
import           Options.Applicative
import           System.Directory
import           System.FilePath
import qualified Audio
import qualified Data.ByteString               as B
import qualified Emulator
import qualified SDL
import qualified Thread.EventLoop              as EventLoop
import qualified Thread.LCD                    as LCD
import qualified Window

data Options = Options
  { debugMode   :: Bool
  , noSound     :: Bool
  , scaleFactor :: Int
  , speedFactor :: Double
  , filename    :: FilePath
  }

optionsP :: Parser Options
optionsP =
  Options
    <$> switch (long "debug" <> help "Enable the debugger")
    <*> switch (long "no-sound" <> help "Disable audio output")
    <*> option
          auto
          (long "scale" <> value 2 <> metavar "SCALE" <> help
            "Default scale factor for video output"
          )
    <*> option
          auto
          (long "speed" <> value 1 <> metavar "SPEED" <> help "Speed as a fraction of normal speed")
    <*> strArgument (metavar "ROM-FILE" <> help "The ROM file to run")

description :: ParserInfo Options
description =
  info (optionsP <**> helper) (fullDesc <> header "hgbc-sdl - a Gameboy Color emulator")

getROMPaths :: FilePath -> IO ROMPaths
getROMPaths romFile = do
  baseDir <- getAppUserDataDirectory "hgbc" <&> (</> "rom")
  let romDir = baseDir </> takeBaseName romFile
  createDirectoryIfMissing True romDir
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
    Right rom -> emulator rom allOptions

emulator :: ROM -> Options -> IO ()
emulator rom Options {..} = do
  graphicsSync          <- newGraphicsSync
  emulatorChannel       <- Emulator.new
  (window, frameBuffer) <- LCD.start (takeBaseName filename) scaleFactor speedFactor graphicsSync
  emulatorState         <- initEmulatorState rom graphicsSync frameBuffer
  audio                 <- if noSound then pure Nothing else Just <$> Audio.start emulatorState
  EventLoop.start window defaultKeymap emulatorChannel emulatorState

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
