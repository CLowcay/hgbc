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
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Keymap
import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import qualified Text.Toml                     as Toml
import qualified Text.Toml.Types               as Toml

type family HKD f a where
  HKD Identity a = a
  HKD f a        = f a

data Config f = Config
  { speed :: HKD f Double
  , scale :: HKD f Int
  , keypad :: HKD f Keymap
  }

deriving instance Eq (Config Identity)
deriving instance Ord (Config Identity)
deriving instance Show (Config Identity)
deriving instance Eq (Config Maybe)
deriving instance Ord (Config Maybe)
deriving instance Show (Config Maybe)

instance Semigroup (Config Maybe) where
  left <> right = Config { speed  = lastOf speed
                         , scale  = lastOf scale
                         , keypad = keypad left <> keypad right
                         }
   where
    lastOf :: (Config Maybe -> Maybe a) -> Maybe a
    lastOf f = getLast . mconcat . fmap Last $ [f left, f right]

instance Monoid (Config Maybe) where
  mempty = Config Nothing Nothing Nothing

finalize :: Config Maybe -> Config Identity
finalize Config {..} = Config { speed  = fromMaybe 1 speed
                              , scale  = fromMaybe 2 scale
                              , keypad = fromMaybe defaultKeymap keypad
                              }

parseConfigFile :: FilePath -> IO (Either [String] (Config Maybe))
parseConfigFile filename = do
  rawContents <- B.readFile filename
  pure (parseConfig filename (T.decodeUtf8With T.lenientDecode rawContents))

parseConfig :: FilePath -> T.Text -> Either [String] (Config Maybe)
parseConfig filename contents = case Toml.parseTomlDoc filename contents of
  Left  parseError -> Left [show parseError]
  Right table      -> decodeConfig table

decodeConfig :: Toml.Table -> Either [String] (Config Maybe)
decodeConfig table = case partitionEithers (decodeRow <$> HM.toList table) of
  ([]    , rows) -> Right (mconcat rows)
  (errors, _   ) -> Left errors

 where
  decodeRow ("speed", Toml.VInteger i) = Right (mempty { speed = Just (fromIntegral i) })
  decodeRow ("speed", Toml.VFloat f  ) = Right (mempty { speed = Just f })
  decodeRow ("scale", Toml.VInteger i) = Right (mempty { scale = Just (fromIntegral i) })
  decodeRow ("keypad", Toml.VTable keypadTable) =
    bimap keypadError (\k -> mempty { keypad = Just k }) (decodeKeymap keypadTable)
  decodeRow (key, value) = Left ("Invalid row " <> show key <> " = " <> show value)

  keypadError = mconcat
    . fmap (\(key, value) -> "Invalid row in keypad section: " <> show key <> " = " <> value)
