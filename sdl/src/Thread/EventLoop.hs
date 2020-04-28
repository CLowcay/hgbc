module Thread.EventLoop
  ( start
  )
where

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Maybe
import qualified HGBC.Emulator
import qualified HGBC.Keymap                   as Keymap
import qualified Machine.GBC                   as GBC
import qualified SDL
import qualified Window

-- | Start the event loop.
start
  :: Window.Window
  -> Keymap.Keymap SDL.Scancode
  -> HGBC.Emulator.Emulator
  -> GBC.EmulatorState
  -> IO ()
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
                case Keymap.lookup keymap (decodeKeysym (SDL.keyboardEventKeysym eventData)) of
                  Nothing                  -> pure ()
                  Just (Keymap.GBCKey key) -> runReaderT (GBC.keyDown key) emulatorState
                  Just Keymap.Pause ->
                    HGBC.Emulator.sendNotification emulator HGBC.Emulator.PauseNotification
                  Just Keymap.Quit ->
                    HGBC.Emulator.sendNotification emulator HGBC.Emulator.QuitNotification
              SDL.Released ->
                case Keymap.lookup keymap (decodeKeysym (SDL.keyboardEventKeysym eventData)) of
                  Just (Keymap.GBCKey key) -> runReaderT (GBC.keyUp key) emulatorState
                  _                        -> pure ()
      SDL.QuitEvent -> HGBC.Emulator.sendNotification emulator HGBC.Emulator.QuitNotification
      payload       -> Window.dispatchNotification isMainWindow payload

    go

-- | Prepare an 'SDL.Keysym' for lookup in the keymap.
decodeKeysym :: SDL.Keysym -> (SDL.Scancode, [Keymap.Modifier])
decodeKeysym (SDL.Keysym scancode _ modifiers) = (scancode, decodeSDLModifiers modifiers)

-- | Decode SDL key modifiers.
decodeSDLModifiers :: SDL.KeyModifier -> [Keymap.Modifier]
decodeSDLModifiers modifiers = catMaybes [control, alt, shift]
 where
  mapTo selectors decodedModifier =
    if any ($ modifiers) selectors then Just decodedModifier else Nothing
  control = [SDL.keyModifierLeftCtrl, SDL.keyModifierRightCtrl] `mapTo` Keymap.Control
  alt     = [SDL.keyModifierLeftAlt, SDL.keyModifierRightAlt] `mapTo` Keymap.Alt
  shift   = [SDL.keyModifierLeftShift, SDL.keyModifierRightShift] `mapTo` Keymap.Shift
