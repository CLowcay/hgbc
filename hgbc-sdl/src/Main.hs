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
import qualified Data.ByteString               as B
import qualified Emulator
import qualified SDL
import qualified Thread.EventLoop              as EventLoop
import qualified Thread.LCD                    as LCD
import qualified Window

data Options = Options
  { debugMode :: Bool
  , filename  :: FilePath
  }

optionsP :: Parser Options
optionsP = Options <$> switch (long "debug" <> help "Enable the debugger") <*> strArgument
  (metavar "ROM-FILE" <> help "The ROM file to run")

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
  Options {..} <- execParser description
  fileContent  <- B.readFile filename
  romPaths     <- getROMPaths filename
  let (eROM, warnings) = runWriter (runExceptT (parseROM romPaths fileContent))

  unless (null warnings) $ do
    putStrLn ("Some problems were detected in " <> filename <> ":")
    for_ warnings $ \message -> putStrLn (" - " <> message)

  case eROM of
    Left err -> do
      putStrLn ("Cannot load " <> filename <> " because:")
      putStrLn (" - " <> err)
    Right rom -> emulator rom

emulator :: ROM -> IO ()
emulator rom = do
  graphicsSync    <- newGraphicsSync
  emulatorChannel <- Emulator.new
  let filename = takeBaseName (romFile (romPaths rom))
  (window, frameBuffer) <- LCD.start filename graphicsSync
  emulatorState         <- initEmulatorState rom graphicsSync frameBuffer
  EventLoop.start window defaultKeymap emulatorChannel emulatorState

  let emulatorLoop = do
        step
        notification <- Emulator.getNotification emulatorChannel
        case notification of
          Nothing                         -> emulatorLoop
          Just Emulator.QuitNotification  -> pure ()
          Just Emulator.PauseNotification -> do
            liftIO $ putStrLn "paused"
            Window.sendNotification window Window.PausedNotification
            notification1 <- Emulator.waitNotification emulatorChannel
            liftIO $ putStrLn ("notification: " <> show notification1)
            case notification1 of
              Emulator.PauseNotification -> do
                liftIO $ putStrLn "resume"
                Window.sendNotification window Window.ResumedNotification
                emulatorLoop
              Emulator.QuitNotification -> pure ()

  runReaderT (reset >> emulatorLoop) emulatorState
