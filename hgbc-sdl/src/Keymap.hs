{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Keymap
  ( Key(..)
  , Modifier(..)
  , Keymap
  , decodeKeymap
  , defaultKeymap
  , decodeKeysym
  )
where

import           Control.Applicative
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           Data.Functor
import qualified Machine.GBC                   as GBC
import qualified SDL
import qualified Text.Toml.Types               as Toml

data Modifier = Control | Alt | Shift
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | A decoded key.
data Key
  = GBCKey GBC.Key
  | Pause
  | Quit
  deriving (Eq, Ord, Show)

-- | A mapping from SDL key symbols to 'Key's.
newtype Keymap = Keymap (M.Map (SDL.Scancode, [Modifier]) Key)
  deriving (Eq, Ord, Show)

instance Semigroup Keymap where
  Keymap left <> Keymap right = Keymap (M.union right left)

instance Monoid Keymap where
  mempty = Keymap mempty

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
  , ((SDL.ScancodeQ, [Control]) , Quit)
  ]

-- | Decode a keymap from TOML, returning the keymap or a list of errors. Each
-- error is a pair of the key that was incorrect and the incorrect value.
decodeKeymap :: Toml.Table -> Either [(String, String)] Keymap
decodeKeymap table = case partitionEithers (decodeRow <$> HM.toList table) of
  ([]    , rows) -> Right (Keymap (M.fromList (concat rows)))
  (errors, _   ) -> Left errors
 where
  decodeRow (key, Toml.VString mapping) = case (,) <$> decodeScancode mapping <*> decodeKey key of
    Nothing  -> Left (show key, show mapping)
    Just row -> Right [row]
  decodeRow (key, Toml.VArray mapping) =
    case (,) <$> (toList <$> traverse decodeScancode' mapping) <*> decodeKey key of
      Nothing                 -> Left (show key, show mapping)
      Just (rows, decodedKey) -> Right (rows <&> (, decodedKey))
  decodeRow (key, value) = Left (show key, show value)
  decodeScancode' (Toml.VString value) = decodeScancode value
  decodeScancode' _                    = Nothing

decodeKey :: T.Text -> Maybe Key
decodeKey text = case text of
  "a"      -> Just (GBCKey GBC.KeyA)
  "b"      -> Just (GBCKey GBC.KeyB)
  "start"  -> Just (GBCKey GBC.KeyStart)
  "select" -> Just (GBCKey GBC.KeySelect)
  "up"     -> Just (GBCKey GBC.KeyUp)
  "down"   -> Just (GBCKey GBC.KeyDown)
  "left"   -> Just (GBCKey GBC.KeyLeft)
  "right"  -> Just (GBCKey GBC.KeyRight)
  "pause"  -> Just Pause
  "quit"   -> Just Quit
  _        -> Nothing

decodeScancode :: T.Text -> Maybe (SDL.Scancode, [Modifier])
decodeScancode "" = Nothing
decodeScancode text =
  (attachModifiers SDL.ScancodeKPMinus =<< T.stripSuffix "KP-" text)
    <|> (attachModifiers SDL.ScancodeMinus =<< T.stripSuffix "-" text)
    <|> (case reverse (T.splitOn "-" (T.toUpper text)) of
          []                -> Nothing
          (key : modifiers) -> (,) <$> decodeMain key <*> traverse decodeModifier modifiers
        )

 where
  attachModifiers key modifiers = (key, ) <$> decodeModifiers modifiers
  decodeModifiers "" = Just []
  decodeModifiers modifiers =
    traverse decodeModifier . T.splitOn "-" =<< T.stripSuffix "-" modifiers

  decodeModifier "C" = Just Control
  decodeModifier "A" = Just Alt
  decodeModifier "S" = Just Shift
  decodeModifier _   = Nothing

  decodeMain "`"           = Just SDL.ScancodeGrave
  decodeMain "1"           = Just SDL.Scancode1
  decodeMain "2"           = Just SDL.Scancode2
  decodeMain "3"           = Just SDL.Scancode3
  decodeMain "4"           = Just SDL.Scancode4
  decodeMain "5"           = Just SDL.Scancode5
  decodeMain "6"           = Just SDL.Scancode6
  decodeMain "7"           = Just SDL.Scancode7
  decodeMain "8"           = Just SDL.Scancode8
  decodeMain "9"           = Just SDL.Scancode9
  decodeMain "0"           = Just SDL.Scancode0
  decodeMain "="           = Just SDL.ScancodeEquals
  decodeMain "Q"           = Just SDL.ScancodeQ
  decodeMain "W"           = Just SDL.ScancodeW
  decodeMain "E"           = Just SDL.ScancodeE
  decodeMain "R"           = Just SDL.ScancodeR
  decodeMain "T"           = Just SDL.ScancodeT
  decodeMain "Y"           = Just SDL.ScancodeY
  decodeMain "U"           = Just SDL.ScancodeU
  decodeMain "I"           = Just SDL.ScancodeI
  decodeMain "O"           = Just SDL.ScancodeO
  decodeMain "P"           = Just SDL.ScancodeP
  decodeMain "["           = Just SDL.ScancodeLeftBracket
  decodeMain "]"           = Just SDL.ScancodeRightBracket
  decodeMain "\\"          = Just SDL.ScancodeBackslash
  decodeMain "A"           = Just SDL.ScancodeA
  decodeMain "S"           = Just SDL.ScancodeS
  decodeMain "D"           = Just SDL.ScancodeD
  decodeMain "F"           = Just SDL.ScancodeF
  decodeMain "G"           = Just SDL.ScancodeG
  decodeMain "H"           = Just SDL.ScancodeH
  decodeMain "J"           = Just SDL.ScancodeJ
  decodeMain "K"           = Just SDL.ScancodeK
  decodeMain "L"           = Just SDL.ScancodeL
  decodeMain ";"           = Just SDL.ScancodeSemicolon
  decodeMain "'"           = Just SDL.ScancodeApostrophe
  decodeMain "Z"           = Just SDL.ScancodeZ
  decodeMain "X"           = Just SDL.ScancodeX
  decodeMain "C"           = Just SDL.ScancodeC
  decodeMain "V"           = Just SDL.ScancodeV
  decodeMain "B"           = Just SDL.ScancodeB
  decodeMain "N"           = Just SDL.ScancodeN
  decodeMain "M"           = Just SDL.ScancodeM
  decodeMain ","           = Just SDL.ScancodeComma
  decodeMain "."           = Just SDL.ScancodePeriod
  decodeMain "/"           = Just SDL.ScancodeSlash
  decodeMain "F1"          = Just SDL.ScancodeF1
  decodeMain "F2"          = Just SDL.ScancodeF2
  decodeMain "F3"          = Just SDL.ScancodeF3
  decodeMain "F4"          = Just SDL.ScancodeF4
  decodeMain "F5"          = Just SDL.ScancodeF5
  decodeMain "F6"          = Just SDL.ScancodeF6
  decodeMain "F7"          = Just SDL.ScancodeF7
  decodeMain "F8"          = Just SDL.ScancodeF8
  decodeMain "F9"          = Just SDL.ScancodeF9
  decodeMain "F10"         = Just SDL.ScancodeF10
  decodeMain "F11"         = Just SDL.ScancodeF11
  decodeMain "F12"         = Just SDL.ScancodeF12
  decodeMain "F13"         = Just SDL.ScancodeF13
  decodeMain "F14"         = Just SDL.ScancodeF14
  decodeMain "F15"         = Just SDL.ScancodeF15
  decodeMain "F16"         = Just SDL.ScancodeF16
  decodeMain "F17"         = Just SDL.ScancodeF17
  decodeMain "F18"         = Just SDL.ScancodeF18
  decodeMain "F19"         = Just SDL.ScancodeF19
  decodeMain "F20"         = Just SDL.ScancodeF20
  decodeMain "F21"         = Just SDL.ScancodeF21
  decodeMain "F22"         = Just SDL.ScancodeF22
  decodeMain "F23"         = Just SDL.ScancodeF23
  decodeMain "F24"         = Just SDL.ScancodeF24
  decodeMain "ESC"         = Just SDL.ScancodeEscape
  decodeMain "TAB"         = Just SDL.ScancodeTab
  decodeMain "LSHIFT"      = Just SDL.ScancodeLShift
  decodeMain "LCONTROL"    = Just SDL.ScancodeLCtrl
  decodeMain "LALT"        = Just SDL.ScancodeLAlt
  decodeMain "BACKSPACE"   = Just SDL.ScancodeBackspace
  decodeMain "ENTER"       = Just SDL.ScancodeReturn
  decodeMain "RSHIFT"      = Just SDL.ScancodeRShift
  decodeMain "RALT"        = Just SDL.ScancodeRAlt
  decodeMain "RCONTROL"    = Just SDL.ScancodeRCtrl
  decodeMain "INSERT"      = Just SDL.ScancodeInsert
  decodeMain "DELETE"      = Just SDL.ScancodeDelete
  decodeMain "HOME"        = Just SDL.ScancodeHome
  decodeMain "END"         = Just SDL.ScancodeEnd
  decodeMain "PAGEUP"      = Just SDL.ScancodePageUp
  decodeMain "PAGEDOWN"    = Just SDL.ScancodePageDown
  decodeMain "UP"          = Just SDL.ScancodeUp
  decodeMain "DOWN"        = Just SDL.ScancodeDown
  decodeMain "LEFT"        = Just SDL.ScancodeLeft
  decodeMain "RIGHT"       = Just SDL.ScancodeRight
  decodeMain "KP0"         = Just SDL.ScancodeKP0
  decodeMain "KP1"         = Just SDL.ScancodeKP1
  decodeMain "KP2"         = Just SDL.ScancodeKP2
  decodeMain "KP3"         = Just SDL.ScancodeKP3
  decodeMain "KP4"         = Just SDL.ScancodeKP4
  decodeMain "KP5"         = Just SDL.ScancodeKP5
  decodeMain "KP6"         = Just SDL.ScancodeKP6
  decodeMain "KP7"         = Just SDL.ScancodeKP7
  decodeMain "KP8"         = Just SDL.ScancodeKP8
  decodeMain "KP9"         = Just SDL.ScancodeKP9
  decodeMain "KP/"         = Just SDL.ScancodeKPDivide
  decodeMain "KP*"         = Just SDL.ScancodeKPMultiply
  decodeMain "KP+"         = Just SDL.ScancodeKPPlus
  decodeMain "KPENTER"     = Just SDL.ScancodeKPEnter
  decodeMain "KP."         = Just SDL.ScancodeKPPeriod
  decodeMain "PAUSE"       = Just SDL.ScancodePause
  decodeMain "PRINTSCREEN" = Just SDL.ScancodePrintScreen
  decodeMain _             = Nothing

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
