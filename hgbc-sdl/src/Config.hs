{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Config
  ( Config(..)
  , parseConfig
  , parseConfigFile
  , finalize
  )
where

import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Bits
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Keymap
import           Machine.GBC.Util
import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import qualified Data.Text.Read                as T
import qualified Data.Vector                   as V
import qualified Text.Toml                     as Toml
import qualified Text.Toml.Types               as Toml

type family HKD f a where
  HKD Identity a = a
  HKD f a        = f a

type Palette = (Word32, Word32, Word32, Word32)

data Config f = Config
  { speed :: HKD f Double
  , scale :: HKD f Int
  , keypad :: HKD f Keymap
  , backgroundPalette :: HKD f Palette
  , sprite1Palette :: HKD f Palette
  , sprite2Palette :: HKD f Palette
  }

deriving instance Eq (Config Identity)
deriving instance Ord (Config Identity)
deriving instance Show (Config Identity)
deriving instance Eq (Config Maybe)
deriving instance Ord (Config Maybe)
deriving instance Show (Config Maybe)

instance Semigroup (Config Maybe) where
  left <> right = Config { speed             = lastOf speed
                         , scale             = lastOf scale
                         , keypad            = keypad left <> keypad right
                         , backgroundPalette = lastOf backgroundPalette
                         , sprite1Palette    = lastOf sprite1Palette
                         , sprite2Palette    = lastOf sprite2Palette
                         }
   where
    lastOf :: (Config Maybe -> Maybe a) -> Maybe a
    lastOf f = getLast . mconcat . fmap Last $ [f left, f right]

instance Monoid (Config Maybe) where
  mempty = Config Nothing Nothing Nothing Nothing Nothing Nothing

finalize :: Config Maybe -> Config Identity
finalize Config {..} = Config { speed             = fromMaybe 1 speed
                              , scale             = fromMaybe 2 scale
                              , keypad            = fromMaybe defaultKeymap keypad
                              , backgroundPalette = fromMaybe defaultPalette backgroundPalette
                              , sprite1Palette    = fromMaybe defaultPalette sprite1Palette
                              , sprite2Palette    = fromMaybe defaultPalette sprite2Palette
                              }
  where defaultPalette = (0x0f380fff, 0x306230ff, 0x8bac0fff, 0x9bbc0fff)

parseConfigFile :: FilePath -> IO (Either [String] (Config Maybe))
parseConfigFile filename = do
  rawContents <- B.readFile filename
  pure (parseConfig filename (T.decodeUtf8With T.lenientDecode rawContents))

parseConfig :: FilePath -> T.Text -> Either [String] (Config Maybe)
parseConfig filename contents = case Toml.parseTomlDoc filename contents of
  Left  parseError -> Left [show parseError]
  Right table      -> decodeConfig table

decodeConfig :: Toml.Table -> Either [String] (Config Maybe)
decodeConfig = decodeTable rootTable
 where
  rootTable ("speed", Toml.VInteger i) = Right (mempty { speed = Just (fromIntegral i) })
  rootTable ("speed", Toml.VFloat f  ) = Right (mempty { speed = Just f })
  rootTable ("scale", Toml.VInteger i) = Right (mempty { scale = Just (fromIntegral i) })
  rootTable ("keypad", Toml.VTable keypadTable) =
    bimap keypadError (\k -> mempty { keypad = Just k }) (decodeKeymap keypadTable)
  rootTable ("colors", Toml.VTable table) = decodeTable colorTable table
  rootTable (key     , value            ) = Left ["Invalid row " <> show key <> " = " <> show value]

  colorTable ("background", Toml.VArray v) = do
    p <- paletteNode =<< decodeArray colorNode v
    pure (mempty { backgroundPalette = Just p })
  colorTable ("sprite1", Toml.VArray v) = do
    p <- paletteNode =<< decodeArray colorNode v
    pure (mempty { sprite1Palette = Just p })
  colorTable ("sprite2", Toml.VArray v) = do
    p <- paletteNode =<< decodeArray colorNode v
    pure (mempty { sprite2Palette = Just p })
  colorTable (key, value) =
    Left ["Invalid row in colors section " <> show key <> " = " <> show value]

  colorNode (Toml.VString t) = decodeColor t
  colorNode x                = Left ("Expected a color, got " <> show x)
  paletteNode [c0, c1, c2, c3] = Right (c0, c1, c2, c3)
  paletteNode x = Left ["A palette must have four colors, not " <> show (length x) <> " colors"]

  keypadError =
    fmap (\(key, value) -> "Invalid row in keypad section: " <> show key <> " = " <> value)

decodeArray :: (Toml.Node -> Either String a) -> V.Vector Toml.Node -> Either [String] [a]
decodeArray decode array = case partitionEithers (decode <$> V.toList array) of
  ([]    , rows) -> Right rows
  (errors, _   ) -> Left errors

decodeTable
  :: Monoid a => ((T.Text, Toml.Node) -> Either [String] a) -> Toml.Table -> Either [String] a
decodeTable decode table = case partitionEithers (decode <$> HM.toList table) of
  ([]    , rows) -> Right (mconcat rows)
  (errors, _   ) -> Left (mconcat errors)

decodeColor :: T.Text -> Either String Word32
decodeColor t = case T.stripPrefix "#" t of
  Nothing   -> Left "Colors must start with #"
  Just code -> case T.hexadecimal code of
    Left  err    -> Left err
    Right (n, r) -> do
      when (r /= "") $ Left "Colors must start with # and contain only valid hexadecimal digits."
      case T.length code of
        3 -> Right (expand3 n)
        6 -> Right (expand6 n)
        8 -> Right n
        x -> Left ("Colors must have 3, 6, or 8 hexadecimal digits, not " <> show x <> " digits.")
 where
  expand3 x =
    let r = x .>>. 8
        g = (x .>>. 4) .&. 0xF
        b = x .&. 0xF
    in  (r .<<. 28)
          .|. (r .<<. 24)
          .|. (g .<<. 20)
          .|. (g .<<. 16)
          .|. (b .<<. 12)
          .|. (b .<<. 8)
          .|. 0xFF
  expand6 x =
    let r = x .>>. 16
        g = (x .>>. 8) .&. 0xFF
        b = x .&. 0xFF
    in  (r .<<. 24) .|. (g .<<. 16) .|. (b .<<. 8) .|. 0xFF
