module Keymap
  ( Key(..)
  , Modifier(..)
  , Keymap
  , defaultKeymap
  , decodeKeysym
  )
where

import           Data.Maybe
import qualified Data.Map.Strict               as M
import qualified Machine.GBC                   as GBC
import qualified SDL

data Modifier = Control | Alt | Shift
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | A decoded key.
data Key
  = GBCKey GBC.Key
  | Pause
  deriving (Eq, Ord, Show)

-- | A mapping from SDL key symbols to 'Key's.
newtype Keymap = Keymap (M.Map (SDL.Scancode, [Modifier]) Key)

-- | The default keymap.
defaultKeymap :: Keymap
defaultKeymap = Keymap $ M.fromList
  [ ((SDL.ScancodeZ, [])        , GBCKey GBC.KeyA)
  , ((SDL.ScancodeX, [])        , GBCKey GBC.KeyB)
  , ((SDL.ScancodeReturn, [])   , GBCKey GBC.KeyStart)
  , ((SDL.ScancodeBackspace, []), GBCKey GBC.KeySelect)
  , ((SDL.ScancodeUp, [])       , GBCKey GBC.KeyUp)
  , ((SDL.ScancodeDown, [])     , GBCKey GBC.KeyDown)
  , ((SDL.ScancodeLeft, [])     , GBCKey GBC.KeyLeft)
  , ((SDL.ScancodeRight, [])    , GBCKey GBC.KeyRight)
  , ((SDL.ScancodePause, [])    , Pause)
  , ((SDL.ScancodeK, [])        , Pause)
  ]

-- | Decode an SDL key symbol into a 'Key'.
decodeKeysym :: Keymap -> SDL.Keysym -> Maybe Key
decodeKeysym (Keymap keys) (SDL.Keysym scancode _ modifiers) =
  M.lookup (scancode, decodeSDLModifiers modifiers) keys

decodeSDLModifiers :: SDL.KeyModifier -> [Modifier]
decodeSDLModifiers modifiers = catMaybes [control, alt, shift]
 where
  mapTo selectors decodedModifier =
    if any ($ modifiers) selectors then Just decodedModifier else Nothing
  control = [SDL.keyModifierLeftCtrl, SDL.keyModifierRightCtrl] `mapTo` Control
  alt     = [SDL.keyModifierLeftAlt, SDL.keyModifierRightAlt] `mapTo` Alt
  shift   = [SDL.keyModifierLeftShift, SDL.keyModifierRightShift] `mapTo` Shift
