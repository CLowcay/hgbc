{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Debugger.Status where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bits
import           GHC.Generics
import           Machine.GBC                    ( EmulatorState(..) )
import           Machine.GBC.CPU
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Graphics           ( GraphicsState(..) )
import           Machine.GBC.Memory
import           Machine.GBC.Primitive
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

  , lcdc7 :: Char
  , lcdc6 :: Char
  , lcdc5 :: Char
  , lcdc4 :: Char
  , lcdc3 :: Char
  , lcdc2 :: Char
  , lcdc1 :: Char
  , lcdc0 :: Char

  , hdma57   :: Char
  , hdma56_0 :: String

  , hdma4 :: String
  , hdma3 :: String
  , hdma2 :: String
  , hdma1 :: String

  , bgp76 :: Char
  , bgp54 :: Char
  , bgp32 :: Char
  , bgp10 :: Char

  , obp076 :: Char
  , obp054 :: Char
  , obp032 :: Char
  , obp010 :: Char

  , obp176 :: Char
  , obp154 :: Char
  , obp132 :: Char
  , obp110 :: Char

  , stat6 :: Char
  , stat5 :: Char
  , stat4 :: Char
  , stat3 :: Char
  , stat2 :: Char
  , stat1_0 :: Char

  , scy :: String
  , wy :: String
  , ly :: String

  , scx :: String
  , wx :: String
  , lyc :: String

  , bcps7   :: Char
  , bcps5_3 :: Char
  , bcps2_1 :: Char
  , bcps0   :: Char

  , ocps7   :: Char
  , ocps5_3 :: Char
  , ocps2_1 :: Char
  , ocps0   :: Char

  , bcpd :: String
  , ocpd :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Status

getStatus :: EmulatorState -> IO Status
getStatus emulatorState = runReaderT go emulatorState
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

    lcdc <- readByte LCDC
    let lcdc7 = toBit (lcdc `testBit` 7)
    let lcdc6 = toBit (lcdc `testBit` 6)
    let lcdc5 = toBit (lcdc `testBit` 5)
    let lcdc4 = toBit (lcdc `testBit` 4)
    let lcdc3 = toBit (lcdc `testBit` 3)
    let lcdc2 = toBit (lcdc `testBit` 2)
    let lcdc1 = toBit (lcdc `testBit` 1)
    let lcdc0 = toBit (lcdc `testBit` 0)

    hdma5 <- readByte HDMA5
    let hdma57   = toBit (hdma5 `testBit` 7)
    let hdma56_0 = formatHex (hdma5 .&. 0x7F)

    hdma4 <- formatHex <$> readByte HDMA4
    hdma3 <- formatHex <$> readByte HDMA3
    hdma2 <- formatHex <$> readByte HDMA2
    hdma1 <- formatHex <$> readByte HDMA1

    bgp   <- readByte BGP
    let bgp76 = formatHex (bgp .>>. 6) !! 1
    let bgp54 = formatHex ((bgp .>>. 4) .&. 3) !! 1
    let bgp32 = formatHex ((bgp .>>. 2) .&. 3) !! 1
    let bgp10 = formatHex (bgp .&. 3) !! 1

    obp0 <- readByte OBP0
    let obp076 = formatHex (obp0 .>>. 6) !! 1
    let obp054 = formatHex ((obp0 .>>. 4) .&. 3) !! 1
    let obp032 = formatHex ((obp0 .>>. 2) .&. 3) !! 1
    let obp010 = formatHex (obp0 .&. 3) !! 1

    obp1 <- readByte OBP1
    let obp176 = formatHex (obp1 .>>. 6) !! 1
    let obp154 = formatHex ((obp1 .>>. 4) .&. 3) !! 1
    let obp132 = formatHex ((obp1 .>>. 2) .&. 3) !! 1
    let obp110 = formatHex (obp1 .&. 3) !! 1

    stat <- readByte STAT
    let stat6   = toBit (stat `testBit` 6)
    let stat5   = toBit (stat `testBit` 5)
    let stat4   = toBit (stat `testBit` 4)
    let stat3   = toBit (stat `testBit` 3)
    let stat2   = toBit (stat `testBit` 2)
    let stat1_0 = formatHex (stat .&. 3) !! 1

    scy  <- formatHex <$> readByte SCY
    wy   <- formatHex <$> readByte WY
    ly   <- formatHex <$> readByte LY
    scx  <- formatHex <$> readByte SCX
    wx   <- formatHex <$> readByte WX
    lyc  <- formatHex <$> readByte LYC

    bcps <- readByte BCPS
    let bcps7   = toBit (bcps `testBit` 7)
    let bcps5_3 = formatHex ((bcps .>>. 3) .&. 7) !! 1
    let bcps2_1 = formatHex ((bcps .>>. 1) .&. 3) !! 1
    let bcps0   = toBit (bcps `testBit` 0)

    ocps <- readByte OCPS
    let ocps7   = toBit (ocps `testBit` 7)
    let ocps5_3 = formatHex ((ocps .>>. 3) .&. 7) !! 1
    let ocps2_1 = formatHex ((ocps .>>. 1) .&. 3) !! 1
    let ocps0   = toBit (ocps `testBit` 0)

    bcpd <- liftIO (formatHex <$> directReadPort (portBCPD (graphicsState emulatorState)))
    ocpd <- liftIO (formatHex <$> directReadPort (portBCPD (graphicsState emulatorState)))

    pure Status { .. }
