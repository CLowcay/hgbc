{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HGBC.Keymap
  ( Key(..)
  , Modifier(..)
  , Keymap(..)
  , ScancodeDecoder
  , decodeKeymap
  , lookupKey
  )
where

import           Data.Either
import           Data.Foldable
import           Data.Functor
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Machine.GBC.Keypad            as GBC
import qualified Text.Toml.Types               as Toml

type ScancodeDecoder k = T.Text -> Maybe (k, [Modifier])

data Modifier = Control | Alt | Shift
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | A decoded key.
data Key
  = GBCKey GBC.Key
  | Pause
  | Quit
  deriving (Eq, Ord, Show)

-- | A mapping from platform specific key codes to GBC 'Key's.
newtype Keymap k = Keymap (M.Map (k, [Modifier]) Key)
  deriving (Eq, Ord, Show)

instance Ord k => Semigroup (Keymap k) where
  Keymap left <> Keymap right = Keymap (M.union right left)

instance Ord k => Monoid (Keymap k) where
  mempty = Keymap mempty


-- | Decode a keymap from TOML, returning the keymap or a list of errors. Each
-- error is a pair of the key that was incorrect and the incorrect value.
decodeKeymap :: Ord k => ScancodeDecoder k -> Toml.Table -> Either [(String, String)] (Keymap k)
decodeKeymap decodeScancode table = case partitionEithers (decodeRow <$> HM.toList table) of
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

-- | Decode an SDL key symbol into a 'Key'.
lookupKey :: Ord k => Keymap k -> (k, [Modifier]) -> Maybe Key
lookupKey (Keymap keys) key = M.lookup key keys
