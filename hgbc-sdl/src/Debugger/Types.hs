{-# LANGUAGE OverloadedStrings #-}

module Debugger.Types
  ( LongAddress(..)
  )
where

import           Data.Bits
import           Data.Hashable
import           Data.Word
import           Data.Aeson
import           Machine.GBC.Util

data LongAddress
  = LongAddress !Word16 !Word16
  deriving (Eq, Ord, Show)

instance Hashable LongAddress where
  hashWithSalt salt (LongAddress bank offset) =
    hashWithSalt salt ((fromIntegral bank .<<. 16) .|. fromIntegral offset :: Int)

instance ToJSON LongAddress where
  toJSON (LongAddress bank address) = object ["offset" .= address, "bank" .= bank]
