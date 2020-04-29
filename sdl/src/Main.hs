module Main
  ( main
  )
where

import           Control.Concurrent
import           Control.Exception              ( displayException )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable
import           Keymap
import           Machine.GBC                    ( newGraphicsSync )
import           Numeric
import qualified Audio
import qualified HGBC.Config                   as Config
import qualified HGBC.Config.CommandLine       as CommandLine
import qualified HGBC.Debugger                 as Debugger
import qualified HGBC.Emulator                 as Emulator
import qualified HGBC.Events                   as Event
import qualified SDL
import qualified Thread.EventLoop              as EventLoop
import qualified Thread.LCD                    as LCD
import qualified Window

main :: IO ()
main = do
  SDL.initializeAll
  (configErrors, options, config) <- Config.load decodeScancode defaultKeymap
  runtimeConfig                   <- Emulator.configure options config
  let romFileName = Emulator.romFileName runtimeConfig
  graphicsSync               <- newGraphicsSync
  (window  , frameBuffer   ) <- LCD.start romFileName config graphicsSync
  (warnings, eEmulatorState) <- Emulator.makeEmulatorState romFileName
                                                           config
                                                           graphicsSync
                                                           frameBuffer

  -- Report errors and warnings.
  case eEmulatorState of
    Right _       -> pure ()
    Left  failure -> putStrLn failure

  for_ warnings     putStrLn
  for_ configErrors printErrors

  -- Run the emulator.
  case eEmulatorState of
    Left  _             -> pure ()
    Right emulatorState -> do
      debuggerErrors <- if CommandLine.debugMode options
        then Debugger.start (Config.debugPort config) runtimeConfig emulatorState
        else pure []

      for_ debuggerErrors printErrors

      EventLoop.start window
                      (Config.keypad config)
                      (Emulator.commandChannel runtimeConfig)
                      emulatorState

      audio <- if CommandLine.noSound options
        then pure Nothing
        else Just <$> Audio.start emulatorState

      void $ forkIO $ forwardEvents window audio =<< Event.waitAction
        (Emulator.eventChannel runtimeConfig)

      runReaderT (Emulator.run runtimeConfig) emulatorState

 where
  printErrors (path, errors) = do
    putStrLn ("Errors in " <> path)
    for_ errors (putStrLn . ("  " <>))

  forwardEvents window audio waitEvent = forever $ do
    event <- waitEvent
    case event of
      Event.Resumed -> do
        maybe (pure ()) Audio.resume audio
        Window.send window Window.Resumed
      Event.Paused -> do
        maybe (pure ()) Audio.pause audio
        Window.send window Window.Paused
      Event.Statistics time clock -> do
        let percentage = showFFloat (Just 2) (fromIntegral clock / (time * 1024 * 1024 * 4)) "%"
        putStrLn
          (show clock <> " clocks in " <> showFFloat (Just 3) time " seconds (" <> percentage <> ")"
          )
      Event.IOWarning ctx err -> putStrLn (ctx <> displayException err)
      _                       -> pure ()
