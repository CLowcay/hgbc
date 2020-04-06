{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Debugger.Status where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bits
import           Data.Functor
import           GHC.Generics
import           Machine.GBC                    ( EmulatorState(..) )
import           Machine.GBC.Audio
import           Machine.GBC.Audio.Common
import           Machine.GBC.Audio.WaveChannel
import           Machine.GBC.CPU
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Graphics           ( GraphicsState(..) )
import           Machine.GBC.Memory
import           Machine.GBC.Primitive
import           Machine.GBC.Registers
import           Machine.GBC.Util
import           Prelude                 hiding ( div )
import qualified Data.Vector                   as V

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
  , cpuMode :: String
  , i :: Char
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

  , romBank :: String
  , ramGate :: Char
  , ramBank :: String

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

  , nr106_4 :: Char
  , nr103   :: Char
  , nr102_0 :: Char
  , nr117_6 :: Char
  , nr115_0 :: String
  , nr127_4 :: Char
  , nr123   :: Char
  , nr122_0 :: Char
  , nr13    :: String
  , nr147   :: Char
  , nr146   :: Char
  , nr142_0 :: Char

  , nr217_6 :: Char
  , nr215_0 :: String
  , nr227_4 :: Char
  , nr223   :: Char
  , nr222_0 :: Char
  , nr23    :: String
  , nr247   :: Char
  , nr246   :: Char
  , nr242_0 :: Char

  , nr307   :: Char
  , nr31    :: String
  , nr326_5 :: Char
  , nr33    :: String
  , nr347   :: Char
  , nr346   :: Char
  , nr342_0 :: Char

  , nr415_0 :: String
  , nr427_4 :: Char
  , nr423   :: Char
  , nr422_0 :: Char
  , nr437_4 :: Char
  , nr433   :: Char
  , nr432_0 :: Char
  , nr447   :: Char
  , nr446   :: Char

  , nr507   :: Char
  , nr506_4 :: Char
  , nr503   :: Char
  , nr502_0 :: Char

  , nr517   :: Char
  , nr516   :: Char
  , nr515   :: Char
  , nr514   :: Char
  , nr513   :: Char
  , nr512   :: Char
  , nr511   :: Char
  , nr510   :: Char

  , nr527   :: Char
  , nr523   :: Char
  , nr522   :: Char
  , nr521   :: Char
  , nr520   :: Char

  , pcm127_4 :: Char
  , pcm123_0 :: Char
  , pcm347_4 :: Char
  , pcm343_0 :: Char

  , wave0 :: String
  , wave1 :: String
  , wave2 :: String
  , wave3 :: String
  , wave4 :: String
  , wave5 :: String
  , wave6 :: String
  , wave7 :: String
  , wave8 :: String
  , wave9 :: String
  , waveA :: String
  , waveB :: String
  , waveC :: String
  , waveD :: String
  , waveE :: String
  , waveF :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Status

getStatus :: EmulatorState -> IO Status
getStatus emulatorState = runReaderT go emulatorState
 where
  highlow x = let s = formatHex x in splitAt 2 s
  getBit n x = if x `testBit` n then '1' else '0'
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
    i            <- testIME <&> \ime -> if ime then 'I' else 'i'
    let z = if isFlagSet flagZ flags then 'Z' else 'z'
    let n = if isFlagSet flagN flags then 'N' else 'n'
    let h = if isFlagSet flagH flags then 'H' else 'h'
    let c = if isFlagSet flagCY flags then 'C' else 'c'
    cpuMode <- getMode <&> \case
      ModeHalt   -> "HALT"
      ModeStop   -> "STOP"
      ModeNormal -> "\xA0RUN"

    p1 <- readByte P1
    let p15 = getBit 5 p1
    let p14 = getBit 4 p1
    let p13 = getBit 3 p1
    let p12 = getBit 2 p1
    let p11 = getBit 1 p1
    let p10 = getBit 0 p1

    div  <- formatHex <$> readByte DIV
    tima <- formatHex <$> readByte TIMA
    tma  <- formatHex <$> readByte TMA

    tac  <- readByte TAC
    let tac2   = getBit 2 tac
    let tac1_0 = formatHex (tac .&. 3) !! 1

    key1 <- readByte KEY1
    let key17 = getBit 7 key1
    let key10 = getBit 0 key1

    vbk <- readByte VBK
    let vbk0 = getBit 0 vbk

    svbk <- readByte SVBK
    let svbk7   = getBit 7 svbk
    let svbk5   = getBit 5 svbk
    let svbk4   = getBit 4 svbk
    let svbk3   = getBit 3 svbk
    let svbk2_0 = formatHex (svbk .&. 7) !! 1

    ie <- readByte IE
    let ie4 = getBit 4 ie
    let ie3 = getBit 3 ie
    let ie2 = getBit 2 ie
    let ie1 = getBit 1 ie
    let ie0 = getBit 0 ie

    rif <- readByte IF
    let if4 = getBit 4 rif
    let if3 = getBit 3 rif
    let if2 = getBit 2 rif
    let if1 = getBit 1 rif
    let if0 = getBit 0 rif

    sb <- readByte SB
    sc <- readByte SC
    rp <- readByte RP

    let sb7   = getBit 7 sb
    let sb6   = getBit 6 sb
    let sb5   = getBit 5 sb
    let sb4   = getBit 4 sb
    let sb3   = getBit 3 sb
    let sb2   = getBit 2 sb
    let sb1   = getBit 1 sb
    let sb0   = getBit 0 sb

    let sc7   = getBit 7 sc
    let sc1   = getBit 1 sc
    let sc0   = getBit 0 sc

    let rp7_6 = formatHex (rp .>>. 6) !! 1
    let rp1   = getBit 1 rp
    let rp0   = getBit 0 rp

    romBank <- formatHex <$> getBank 0x4000
    ramGate <- getRamGate <&> \g -> if g then 'O' else 'C'
    ramBank <- formatHex <$> getBank 0xA000

    lcdc    <- readByte LCDC
    let lcdc7 = getBit 7 lcdc
    let lcdc6 = getBit 6 lcdc
    let lcdc5 = getBit 5 lcdc
    let lcdc4 = getBit 4 lcdc
    let lcdc3 = getBit 3 lcdc
    let lcdc2 = getBit 2 lcdc
    let lcdc1 = getBit 1 lcdc
    let lcdc0 = getBit 0 lcdc

    hdma5 <- readByte HDMA5
    let hdma57   = getBit 7 hdma5
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
    let stat6   = getBit 6 stat
    let stat5   = getBit 5 stat
    let stat4   = getBit 4 stat
    let stat3   = getBit 3 stat
    let stat2   = getBit 2 stat
    let stat1_0 = formatHex (stat .&. 3) !! 1

    scy  <- formatHex <$> readByte SCY
    wy   <- formatHex <$> readByte WY
    ly   <- formatHex <$> readByte LY
    scx  <- formatHex <$> readByte SCX
    wx   <- formatHex <$> readByte WX
    lyc  <- formatHex <$> readByte LYC

    bcps <- readByte BCPS
    let bcps7   = getBit 7 bcps
    let bcps5_3 = formatHex ((bcps .>>. 3) .&. 7) !! 1
    let bcps2_1 = formatHex ((bcps .>>. 1) .&. 3) !! 1
    let bcps0   = getBit 0 bcps

    ocps <- readByte OCPS
    let ocps7   = getBit 7 ocps
    let ocps5_3 = formatHex ((ocps .>>. 3) .&. 7) !! 1
    let ocps2_1 = formatHex ((ocps .>>. 1) .&. 3) !! 1
    let ocps0   = getBit 0 ocps

    bcpd <- liftIO (formatHex <$> directReadPort (portBCPD (graphicsState emulatorState)))
    ocpd <- liftIO (formatHex <$> directReadPort (portBCPD (graphicsState emulatorState)))

    (nr10, nr11, nr12, nr13raw, nr14) <- liftIO
      (directReadPorts (channel1 . audioState $ emulatorState))
    let nr13    = formatHex nr13raw
    let nr106_4 = formatHex ((nr10 .>>. 4) .&. 7) !! 1
    let nr103   = getBit 3 nr10
    let nr102_0 = formatHex (nr10 .&. 7) !! 1
    let nr117_6 = formatHex (nr11 .>>. 6) !! 1
    let nr115_0 = formatHex (nr11 .&. 0x3F)
    let nr127_4 = head (formatHex nr12)
    let nr123   = getBit 3 nr12
    let nr122_0 = formatHex (nr12 .&. 7) !! 1
    let nr147   = getBit 7 nr14
    let nr146   = getBit 6 nr14
    let nr142_0 = formatHex (nr14 .&. 7) !! 1

    (_, nr21, nr22, nr23raw, nr24) <- liftIO
      (directReadPorts (channel2 . audioState $ emulatorState))

    let nr23    = formatHex nr23raw
    let nr217_6 = formatHex (nr21 .>>. 6) !! 1
    let nr215_0 = formatHex (nr21 .&. 0x3F)
    let nr227_4 = head (formatHex nr22)
    let nr223   = getBit 3 nr22
    let nr222_0 = formatHex (nr22 .&. 7) !! 1
    let nr247   = getBit 7 nr24
    let nr246   = getBit 6 nr24
    let nr242_0 = formatHex (nr24 .&. 7) !! 1

    (nr30, nr31raw, nr32, nr33raw, nr34) <- liftIO
      (directReadPorts (channel3 . audioState $ emulatorState))
    let nr31    = formatHex nr31raw
    let nr33    = formatHex nr33raw
    let nr307   = getBit 7 nr30
    let nr326_5 = formatHex ((nr32 .>>. 5) .&. 3) !! 1
    let nr347   = getBit 7 nr34
    let nr346   = getBit 6 nr34
    let nr342_0 = formatHex (nr34 .&. 7) !! 1

    (_, nr41, nr42, nr43, nr44) <- liftIO (directReadPorts (channel4 . audioState $ emulatorState))
    let nr415_0 = formatHex (nr41 .&. 0x3F)
    let nr427_4 = head (formatHex nr42)
    let nr423   = getBit 3 nr42
    let nr422_0 = formatHex (nr42 .&. 7) !! 1
    let nr437_4 = head (formatHex nr43)
    let nr433   = getBit 3 nr43
    let nr432_0 = formatHex (nr43 .&. 7) !! 1
    let nr447   = getBit 7 nr44
    let nr446   = getBit 6 nr44

    nr50  <- readByte NR50
    nr51  <- readByte NR51
    nr52  <- readByte NR52
    pcm12 <- readByte PCM12
    pcm34 <- readByte PCM34

    let nr507     = getBit 7 nr50
    let nr506_4   = head (formatHex (nr50 .&. 0x70))
    let nr503     = getBit 3 nr50
    let nr502_0   = formatHex (nr50 .&. 0x07) !! 1

    let nr517     = getBit 7 nr51
    let nr516     = getBit 6 nr51
    let nr515     = getBit 5 nr51
    let nr514     = getBit 4 nr51
    let nr513     = getBit 3 nr51
    let nr512     = getBit 2 nr51
    let nr511     = getBit 1 nr51
    let nr510     = getBit 0 nr51

    let nr527     = getBit 7 nr52
    let nr523     = getBit 3 nr52
    let nr522     = getBit 2 nr52
    let nr521     = getBit 1 nr52
    let nr520     = getBit 0 nr52

    let pcm127_4  = head (formatHex pcm12)
    let pcm123_0  = formatHex pcm12 !! 1

    let pcm347_4  = head (formatHex pcm34)
    let pcm343_0  = formatHex pcm12 !! 1

    let waveTable = portWaveTable . channel3 . audioState $ emulatorState

    wave0 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x0))
    wave1 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x1))
    wave2 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x2))
    wave3 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x3))
    wave4 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x4))
    wave5 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x5))
    wave6 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x6))
    wave7 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x7))
    wave8 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x8))
    wave9 <- liftIO (formatHex <$> directReadPort (waveTable V.! 0x9))
    waveA <- liftIO (formatHex <$> directReadPort (waveTable V.! 0xA))
    waveB <- liftIO (formatHex <$> directReadPort (waveTable V.! 0xB))
    waveC <- liftIO (formatHex <$> directReadPort (waveTable V.! 0xC))
    waveD <- liftIO (formatHex <$> directReadPort (waveTable V.! 0xD))
    waveE <- liftIO (formatHex <$> directReadPort (waveTable V.! 0xE))
    waveF <- liftIO (formatHex <$> directReadPort (waveTable V.! 0xF))

    pure Status { .. }
