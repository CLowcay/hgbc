{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HGBC.Config.File
  ( Config(..)
  , parse
  , parseFile
  )
where

import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Either
import           Data.Monoid
import           Data.Word
import           HGBC.Config.Decode
import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import qualified Data.Vector                   as V
import qualified HGBC.Keymap                   as Keymap
import qualified Machine.GBC                   as GBC
import qualified Text.Toml                     as Toml
import qualified Text.Toml.Types               as Toml

type family HKD f a where
  HKD Identity a      = a
  HKD Maybe (Maybe a) = Maybe a
  HKD f a             = f a

type Palette = (Word32, Word32, Word32, Word32)

data Config k f = Config
  { speed :: HKD f Double
  , scale :: HKD f Int
  , noVsync :: HKD f Bool
  , debugPort :: HKD f Int
  , bootROM :: HKD f (Maybe FilePath)
  , mode :: HKD f (Maybe GBC.EmulatorMode)
  , colorCorrection :: HKD f GBC.ColorCorrection
  , keypad :: HKD f (Keymap.Keymap k)
  , backgroundPalette :: HKD f Palette
  , sprite1Palette :: HKD f Palette
  , sprite2Palette :: HKD f Palette
  }

deriving instance Eq k => Eq (Config k Identity)
deriving instance Ord k => Ord (Config k Identity)
deriving instance Show k => Show (Config k Identity)
deriving instance Eq k => Eq (Config k Maybe)
deriving instance Ord k => Ord (Config k Maybe)
deriving instance Show k => Show (Config k Maybe)

instance Ord k => Semigroup (Config k Maybe) where
  left <> right = Config { speed             = lastOf speed
                         , scale             = lastOf scale
                         , noVsync           = lastOf noVsync
                         , debugPort         = lastOf debugPort
                         , bootROM           = lastOf bootROM
                         , colorCorrection   = lastOf colorCorrection
                         , mode              = lastOf mode
                         , keypad            = keypad left <> keypad right
                         , backgroundPalette = lastOf backgroundPalette
                         , sprite1Palette    = lastOf sprite1Palette
                         , sprite2Palette    = lastOf sprite2Palette
                         }
   where
    lastOf :: (Config k Maybe -> Maybe a) -> Maybe a
    lastOf f = getLast . mconcat . fmap Last $ [f left, f right]

instance Ord k => Monoid (Config k Maybe) where
  mempty =
    Config Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parseFile :: Ord k => Keymap.ScancodeDecoder k -> FilePath -> IO (Either [String] (Config k Maybe))
parseFile decodeScancode filename = do
  rawContents <- B.readFile filename
  pure (parse decodeScancode filename (T.decodeUtf8With T.lenientDecode rawContents))

parse :: Ord k => Keymap.ScancodeDecoder k -> FilePath -> T.Text -> Either [String] (Config k Maybe)
parse decodeScancode filename contents = case Toml.parseTomlDoc filename contents of
  Left  parseError -> Left [show parseError]
  Right table      -> decodeConfig decodeScancode table

decodeConfig :: Ord k => Keymap.ScancodeDecoder k -> Toml.Table -> Either [String] (Config k Maybe)
decodeConfig decodeScancode = decodeTable rootTable
 where
  rootTable ("speed"     , Toml.VInteger i) = Right (mempty { speed = Just (fromIntegral i) })
  rootTable ("speed"     , Toml.VFloat f  ) = Right (mempty { speed = Just f })
  rootTable ("scale"     , Toml.VInteger i) = Right (mempty { scale = Just (fromIntegral i) })
  rootTable ("no-vsync"  , Toml.VBoolean v) = Right (mempty { noVsync = Just v })
  rootTable ("debug-port", Toml.VInteger i) = Right (mempty { debugPort = Just (fromIntegral i) })
  rootTable ("boot-rom", Toml.VString filename) =
    Right (mempty { bootROM = Just (T.unpack filename) })
  rootTable ("mode", Toml.VString t) = do
    v <- first pure (decodeMode t)
    pure (mempty { mode = v })
  rootTable ("color-correction", Toml.VString t) = do
    v <- first pure (decodeColorCorrection t)
    pure (mempty { colorCorrection = Just v })
  rootTable ("keypad", Toml.VTable keypadTable) =
    bimap keypadError (setKeymap mempty) (Keymap.decode decodeScancode keypadTable)
  rootTable ("colors", Toml.VTable table) = decodeTable colorTable table
  rootTable (key     , v                ) = Left ["Invalid row " <> show key <> " = " <> show v]

  setKeymap :: Config k Maybe -> Keymap.Keymap k -> Config k Maybe
  setKeymap config keymap = config { keypad = Just keymap }

  colorTable ("background", Toml.VArray v) = do
    p <- paletteNode =<< decodeArray colorNode v
    pure (mempty { backgroundPalette = Just p })
  colorTable ("sprite1", Toml.VArray v) = do
    p <- paletteNode =<< decodeArray colorNode v
    pure (mempty { sprite1Palette = Just p })
  colorTable ("sprite2", Toml.VArray v) = do
    p <- paletteNode =<< decodeArray colorNode v
    pure (mempty { sprite2Palette = Just p })
  colorTable (key, v) = Left ["Invalid row in colors section " <> show key <> " = " <> show v]

  colorNode (Toml.VString t) = decodeColor t
  colorNode x                = Left ("Expected a color, got " <> show x)
  paletteNode [c0, c1, c2, c3] = Right (c0, c1, c2, c3)
  paletteNode x = Left ["A palette must have four colors, not " <> show (length x) <> " colors"]

  keypadError = fmap (\(key, v) -> "Invalid row in keypad section: " <> show key <> " = " <> v)

decodeArray :: (Toml.Node -> Either String a) -> V.Vector Toml.Node -> Either [String] [a]
decodeArray f array = case partitionEithers (f <$> V.toList array) of
  ([]    , rows) -> Right rows
  (errors, _   ) -> Left errors

decodeTable
  :: Monoid a => ((T.Text, Toml.Node) -> Either [String] a) -> Toml.Table -> Either [String] a
decodeTable f table = case partitionEithers (f <$> HM.toList table) of
  ([]    , rows) -> Right (mconcat rows)
  (errors, _   ) -> Left (mconcat errors)
