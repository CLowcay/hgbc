{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Debugger.Status where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bits
import           GHC.Generics
import           Machine.GBC                    ( EmulatorState )
import           Machine.GBC.CPU
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Memory
import           Machine.GBC.Registers
import           Machine.GBC.Util
import           Prelude                 hiding ( div )

data Status = Status {
  -- CPU registers
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
  , z :: Char
  , n :: Char
  , h :: Char
  , c :: Char

  -- P1
  , p15 :: Char
  , p14 :: Char
  , p13 :: Char
  , p12 :: Char
  , p11 :: Char
  , p10 :: Char

  , div    :: String
  , tima   :: String
  , tma    :: String
  , tac2   :: Char
  , tac1_0 :: Char

  , key17   :: Char
  , key10   :: Char

  , vbk0    :: Char

  , svbk7   :: Char
  , svbk5   :: Char
  , svbk4   :: Char
  , svbk3   :: Char
  , svbk2_0 :: Char

  , if4     :: Char
  , if3     :: Char
  , if2     :: Char
  , if1     :: Char
  , if0     :: Char

  , ie4     :: Char
  , ie3     :: Char
  , ie2     :: Char
  , ie1     :: Char
  , ie0     :: Char

  , sb7     :: Char
  , sb6     :: Char
  , sb5     :: Char
  , sb4     :: Char
  , sb3     :: Char
  , sb2     :: Char
  , sb1     :: Char
  , sb0     :: Char

  , sc7     :: Char
  , sc1     :: Char
  , sc0     :: Char

  , rp7_6   :: Char
  , rp1     :: Char
  , rp0     :: Char

  , r4c     :: String
  , r6c0    :: Char
  } deriving (Eq, Show, Generic)

instance ToJSON Status

getStatus :: EmulatorState -> IO Status
getStatus = runReaderT go
 where
  highlow x = let s = formatHex x in splitAt 2 s
  toBit x = if x then '1' else '0'
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
    flags        <- readF
    let z = if isFlagSet flagZ flags then 'Z' else 'z'
    let n = if isFlagSet flagN flags then 'N' else 'n'
    let h = if isFlagSet flagH flags then 'H' else 'h'
    let c = if isFlagSet flagCY flags then 'C' else 'c'

    p1 <- readByte P1
    let p15 = toBit (p1 `testBit` 5)
    let p14 = toBit (p1 `testBit` 4)
    let p13 = toBit (p1 `testBit` 3)
    let p12 = toBit (p1 `testBit` 2)
    let p11 = toBit (p1 `testBit` 1)
    let p10 = toBit (p1 `testBit` 0)

    div  <- formatHex <$> readByte DIV
    tima <- formatHex <$> readByte TIMA
    tma  <- formatHex <$> readByte TMA

    tac  <- readByte TAC
    let tac2   = toBit (tac `testBit` 2)
    let tac1_0 = formatHex (tac .&. 3) !! 1

    key1 <- readByte KEY1
    let key17 = toBit (key1 `testBit` 7)
    let key10 = toBit (key1 `testBit` 0)

    vbk <- readByte VBK
    let vbk0 = toBit (vbk `testBit` 0)

    svbk <- readByte SVBK
    let svbk7   = toBit (svbk `testBit` 7)
    let svbk5   = toBit (svbk `testBit` 5)
    let svbk4   = toBit (svbk `testBit` 4)
    let svbk3   = toBit (svbk `testBit` 3)
    let svbk2_0 = formatHex (svbk .&. 7) !! 1

    ie <- readByte IE
    let ie4 = toBit (ie `testBit` 4)
    let ie3 = toBit (ie `testBit` 3)
    let ie2 = toBit (ie `testBit` 2)
    let ie1 = toBit (ie `testBit` 1)
    let ie0 = toBit (ie `testBit` 0)

    rif <- readByte IF
    let if4 = toBit (rif `testBit` 4)
    let if3 = toBit (rif `testBit` 3)
    let if2 = toBit (rif `testBit` 2)
    let if1 = toBit (rif `testBit` 1)
    let if0 = toBit (rif `testBit` 0)

    sb  <- readByte SB
    sc  <- readByte SC
    rp  <- readByte RP
    r4c <- formatHex <$> readByte R4C
    r6c <- readByte R6C

    let sb7   = toBit (sb `testBit` 7)
    let sb6   = toBit (sb `testBit` 6)
    let sb5   = toBit (sb `testBit` 5)
    let sb4   = toBit (sb `testBit` 4)
    let sb3   = toBit (sb `testBit` 3)
    let sb2   = toBit (sb `testBit` 2)
    let sb1   = toBit (sb `testBit` 1)
    let sb0   = toBit (sb `testBit` 0)

    let sc7   = toBit (sc `testBit` 7)
    let sc1   = toBit (sc `testBit` 1)
    let sc0   = toBit (sc `testBit` 0)

    let rp7_6 = formatHex (rp .>>. 6) !! 1
    let rp1   = toBit (rp `testBit` 1)
    let rp0   = toBit (rp `testBit` 0)

    let r6c0  = toBit (r6c `testBit` 0)

    pure Status { .. }
