module Thread.EventLoop
  ( start
  )
where

import           Control.Monad.Reader
import           Control.Concurrent
import           Machine.GBC                    ( EmulatorState
                                                , keyUp
                                                , keyDown
                                                )
import           Keymap
import qualified Emulator
import qualified SDL
import qualified Window
import           Data.Maybe

-- | Start the event loop.
start :: Window.Window -> Keymap SDL.Scancode -> Emulator.Emulator -> EmulatorState -> IO ()
start window keymap emulator emulatorState = void $ forkOS go
 where
  isMainWindow w = if Window.sdlWindow window == w then Just window else Nothing
  go = do
    event <- SDL.eventPayload <$> SDL.waitEvent

    case event of
      SDL.KeyboardEvent eventData ->
        when (SDL.keyboardEventWindow eventData == Just (Window.sdlWindow window))
          $ case SDL.keyboardEventKeyMotion eventData of
              SDL.Pressed ->
                case lookupKey keymap (decodeKeysym (SDL.keyboardEventKeysym eventData)) of
                  Nothing           -> pure ()
                  Just (GBCKey key) -> runReaderT (keyDown key) emulatorState
                  Just Pause        -> Emulator.sendNotification emulator Emulator.PauseNotification
                  Just Quit         -> Emulator.sendNotification emulator Emulator.QuitNotification
              SDL.Released ->
                case lookupKey keymap (decodeKeysym (SDL.keyboardEventKeysym eventData)) of
                  Just (GBCKey key) -> runReaderT (keyUp key) emulatorState
                  _                 -> pure ()
      SDL.QuitEvent -> Emulator.sendNotification emulator Emulator.QuitNotification
      payload       -> Window.dispatchNotification isMainWindow payload

    go

-- | Prepare an 'SDL.Keysym' for lookup in the keymap.
decodeKeysym :: SDL.Keysym -> (SDL.Scancode, [Modifier])
decodeKeysym (SDL.Keysym scancode _ modifiers) = (scancode, decodeSDLModifiers modifiers)

-- | Decode SDL key modifiers.
decodeSDLModifiers :: SDL.KeyModifier -> [Modifier]
decodeSDLModifiers modifiers = catMaybes [control, alt, shift]
 where
  mapTo selectors decodedModifier =
    if any ($ modifiers) selectors then Just decodedModifier else Nothing
  control = [SDL.keyModifierLeftCtrl, SDL.keyModifierRightCtrl] `mapTo` Control
  alt     = [SDL.keyModifierLeftAlt, SDL.keyModifierRightAlt] `mapTo` Alt
  shift   = [SDL.keyModifierLeftShift, SDL.keyModifierRightShift] `mapTo` Shift
