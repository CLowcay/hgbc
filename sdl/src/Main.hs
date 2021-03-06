module Main
  ( main,
  )
where

import qualified Audio
import Control.Concurrent (forkIO)
import Control.Exception (displayException)
import Control.Monad.Except (MonadIO (liftIO), forever, runExceptT, void, when)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Foldable (for_)
import qualified HGBC.Config as Config
import qualified HGBC.Config.CommandLine as CommandLine
import qualified HGBC.Debugger as Debugger
import qualified HGBC.Debugger.ROM as ROM
import qualified HGBC.Emulator as Emulator
import qualified HGBC.Events as Event
import Keymap (decodeScancode, defaultKeymap)
import qualified Machine.GBC.CPU as CPU
import qualified Machine.GBC.Emulator as Emulator
import qualified Machine.GBC.Graphics as Graphics
import Machine.GBC.Memory (getROMHeader)
import qualified Machine.GBC.Memory as Memory
import Machine.GBC.Util (formatHex)
import Numeric (showFFloat)
import qualified SDL
import qualified Thread.EventLoop as EventLoop
import qualified Thread.LCD as LCD
import qualified Window

main :: IO ()
main = do
  SDL.initializeAll
  (configErrors, options, config) <- Config.load decodeScancode defaultKeymap
  runtimeConfig <- Emulator.configure options config
  let romFileName = Emulator.romFileName runtimeConfig
  graphicsSync <- Graphics.newSync
  (window, frameBuffer) <- LCD.start romFileName config graphicsSync
  (eEmulatorState, warnings) <-
    runWriterT
      (runExceptT (Emulator.makeEmulatorState romFileName config graphicsSync frameBuffer))

  -- Report errors and warnings.
  either printErrors (const (pure ())) eEmulatorState
  for_ warnings printErrors
  for_ configErrors printErrors

  -- Run the emulator.
  case eEmulatorState of
    Left _ -> pure ()
    Right emulatorState -> do
      when (CommandLine.debugMode options) $ do
        errors <- Debugger.start (Config.debugPort config) runtimeConfig emulatorState
        for_ errors printErrors

      ROM.dumpHeader (getROMHeader (Emulator.memory emulatorState))

      EventLoop.start
        window
        (Config.keypad config)
        (Emulator.commandChannel runtimeConfig)
        emulatorState

      audio <-
        if CommandLine.noSound options
          then pure Nothing
          else Just <$> Audio.start emulatorState

      forwardEvents window audio emulatorState (Emulator.eventChannel runtimeConfig)
      runReaderT (Emulator.run runtimeConfig) emulatorState
      maybe (pure ()) Audio.pause audio
  where
    printErrors (path, errors) = do
      putStrLn (show (length errors) ++ " errors in " <> path)
      for_ errors (putStrLn . ("  " <>))

    forwardEvents window audio emulatorState eventChannel = do
      waitEvent <- Event.waitAction eventChannel
      void $
        forkIO $
          forever $ do
            event <- waitEvent
            case event of
              Event.Resumed -> do
                maybe (pure ()) Audio.resume audio
                Window.send window Window.Resumed
              Event.Paused -> do
                maybe (pure ()) Audio.pause audio
                Window.send window Window.Paused
              Event.Fault fault -> do
                Window.send window Window.Fault
                flip runReaderT emulatorState $ do
                  pc <- CPU.readPC
                  bank <- Memory.getBank pc
                  liftIO
                    ( putStrLn
                        ( "Fault at "
                            <> formatHex bank
                            <> ":"
                            <> formatHex pc
                            <> " "
                            <> displayException fault
                        )
                    )
              Event.Statistics time clock -> do
                let percentage =
                      showFFloat (Just 2) (fromIntegral clock / (time * 1024 * 1024 * 4) * 100) "%"
                putStrLn
                  ( show clock
                      <> " clocks in "
                      <> showFFloat (Just 3) time " seconds ("
                      <> percentage
                      <> ")"
                  )
              Event.IOWarning ctx err -> putStrLn (ctx <> displayException err)
              _ -> pure ()
