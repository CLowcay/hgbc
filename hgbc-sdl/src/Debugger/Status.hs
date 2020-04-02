{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Debugger.Status where

import           Control.Monad.Reader
import           Data.Aeson
import           GHC.Generics
import           Machine.GBC                    ( EmulatorState )
import           Machine.GBC.CPU
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Util

data Status = Status {
    rA :: String
  , rB :: String
  , rC :: String
  , rD :: String
  , rE :: String
  , rF :: String
  , rH :: String
  , rL :: String
  , rSPL :: String
  , rSPH :: String
  , rPCL :: String
  , rPCH :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Status

getStatus :: EmulatorState -> IO Status
getStatus = runReaderT go
 where
  highlow x = let s = formatHex x in splitAt 2 s
  go = do
    (rA, rF)     <- highlow <$> readR16pp PushPopAF
    rB           <- formatHex <$> readR8 RegB
    rC           <- formatHex <$> readR8 RegC
    rD           <- formatHex <$> readR8 RegD
    rE           <- formatHex <$> readR8 RegE
    rH           <- formatHex <$> readR8 RegH
    rL           <- formatHex <$> readR8 RegL
    (rPCH, rPCL) <- highlow <$> readPC
    (rSPH, rSPL) <- highlow <$> readR16 RegSP
    pure Status { .. }
