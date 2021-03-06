module Thread.EventLoop
  ( start,
  )
where

import Control.Concurrent (forkOS)
import Control.Monad.Reader (void, when)
import Data.Maybe (catMaybes)
import qualified HGBC.Emulator as Emulator
import qualified HGBC.Keymap as Keymap
import qualified Machine.GBC.Emulator as Emulator
import qualified SDL
import qualified Window

-- | Start the event loop.
start :: Window.Window -> Keymap.Keymap SDL.Scancode -> Emulator.Channel -> Emulator.State -> IO ()
start window keymap commandChannel emulatorState = void $ forkOS go
  where
    isMainWindow w = if Window.sdlWindow window == w then Just window else Nothing
    go = do
      event <- SDL.eventPayload <$> SDL.waitEvent

      case event of
        SDL.KeyboardEvent eventData ->
          when (SDL.keyboardEventWindow eventData == Just (Window.sdlWindow window)) $
            case SDL.keyboardEventKeyMotion eventData of
              SDL.Pressed ->
                case Keymap.lookup keymap (decodeKeysym (SDL.keyboardEventKeysym eventData)) of
                  Nothing -> pure ()
                  Just (Keymap.GBCKey key) -> Emulator.keyDown emulatorState key
                  Just Keymap.Pause -> Emulator.send commandChannel Emulator.Pause
                  Just Keymap.Quit -> Emulator.send commandChannel Emulator.Quit
              SDL.Released ->
                case Keymap.lookup keymap (decodeKeysym (SDL.keyboardEventKeysym eventData)) of
                  Just (Keymap.GBCKey key) -> Emulator.keyUp emulatorState key
                  _ -> pure ()
        SDL.QuitEvent -> Emulator.send commandChannel Emulator.Quit
        payload -> Window.dispatchEvent isMainWindow payload

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
    alt = [SDL.keyModifierLeftAlt, SDL.keyModifierRightAlt] `mapTo` Keymap.Alt
    shift = [SDL.keyModifierLeftShift, SDL.keyModifierRightShift] `mapTo` Keymap.Shift
