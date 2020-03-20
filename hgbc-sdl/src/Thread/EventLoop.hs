module Thread.EventLoop
  ( start
  )
where

import           Control.Monad.Reader
import           Control.Concurrent
import           Machine.GBC
import           Keymap
import qualified Emulator
import qualified SDL
import qualified Window

-- | Start the event loop.
start :: Window.Window -> Keymap -> Emulator.Emulator -> EmulatorState -> IO ()
start window keymap emulator emulatorState = void $ forkOS go
 where
  isMainWindow w = if Window.sdlWindow window == w then Just window else Nothing
  go = do
    event <- SDL.eventPayload <$> SDL.waitEvent

    case event of
      SDL.KeyboardEvent eventData ->
        when (SDL.keyboardEventWindow eventData == Just (Window.sdlWindow window))
          $ case SDL.keyboardEventKeyMotion eventData of
              SDL.Pressed -> case decodeKeysym keymap (SDL.keyboardEventKeysym eventData) of
                Just (GBCKey key) -> runReaderT (keyDown key) emulatorState
                _                 -> pure ()
              SDL.Released -> case decodeKeysym keymap (SDL.keyboardEventKeysym eventData) of
                Nothing           -> pure ()
                Just (GBCKey key) -> runReaderT (keyUp key) emulatorState
                Just Pause        -> Emulator.sendNotification emulator Emulator.PauseNotification
      SDL.QuitEvent -> Emulator.sendNotification emulator Emulator.QuitNotification
      payload       -> Window.dispatchNotification isMainWindow payload

    go
