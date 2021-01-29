{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.Status
  ( getStatus,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Aeson (KeyValue ((.=)), Series, fromEncoding, pairs)
import Data.Bits (Bits (testBit, (.&.)))
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Machine.GBC.Audio as Audio
import Machine.GBC.Audio.Common (directReadPorts)
import Machine.GBC.Audio.WaveChannel (WaveChannel (..))
import qualified Machine.GBC.CPU as CPU
import Machine.GBC.CPU.ISA (Register16 (RegSP), Register8 (RegB, RegC, RegD, RegE, RegH, RegL), RegisterPushPop (PushPopAF))
import qualified Machine.GBC.Emulator as Emulator
import qualified Machine.GBC.Graphics as Graphics
import Machine.GBC.Memory (getBank, getRamGate, readByte)
import qualified Machine.GBC.Primitive.Port as Port
import qualified Machine.GBC.Registers as R
import Machine.GBC.Util (formatHex, isFlagSet, (.>>.))
import Prelude hiding (div)

getStatus :: Emulator.State -> IO LBS.ByteString
getStatus emulatorState =
  BB.toLazyByteString . fromEncoding . pairs <$> runReaderT go emulatorState
  where
    highlow x = let s = formatHex x in splitAt 2 s
    getBit n x = if x `testBit` n then '1' else '0'
    go :: ReaderT Emulator.State IO Series
    go =
      fmap (foldl' (<>) mempty)
        . foldl' (liftA2 (++)) (pure [])
        $ [ rAF,
            field "rB" formatHex <$> CPU.readR8 RegB,
            field "rC" formatHex <$> CPU.readR8 RegC,
            field "rD" formatHex <$> CPU.readR8 RegD,
            field "rE" formatHex <$> CPU.readR8 RegE,
            field "rH" formatHex <$> CPU.readR8 RegH,
            field "rL" formatHex <$> CPU.readR8 RegL,
            rPC,
            rSP,
            flags,
            p1,
            field "div" formatHex <$> readByte R.DIV,
            field "tima" formatHex <$> readByte R.TIMA,
            field "tma" formatHex <$> readByte R.TMA,
            tac,
            key1,
            field "vbk0" (getBit 0) <$> readByte R.VBK,
            svbk,
            ie,
            rif,
            sc,
            allBitsOf "sb" <$> readByte R.SB,
            rp,
            romBanks,
            field "ramGate" (\g -> if g then 'O' else 'C') <$> getRamGate,
            field "ramBank" formatHex <$> getBank 0xA000,
            allBitsOf "lcdc" <$> readByte R.LCDC,
            field "dma" formatHex <$> readByte R.DMA,
            hdma5,
            field "hdma4" formatHex <$> readByte R.HDMA4,
            field "hdma3" formatHex <$> readByte R.HDMA3,
            field "hdma2" formatHex <$> readByte R.HDMA2,
            field "hdma1" formatHex <$> readByte R.HDMA1,
            dmgPalette "bgp" <$> readByte R.BGP,
            dmgPalette "obp0" <$> readByte R.OBP0,
            dmgPalette "obp1" <$> readByte R.OBP1,
            stat,
            field "scy" formatHex <$> readByte R.SCY,
            field "wy" formatHex <$> readByte R.WY,
            field "ly" formatHex <$> readByte R.LY,
            field "scx" formatHex <$> readByte R.SCX,
            field "wx" formatHex <$> readByte R.WX,
            field "lyc" formatHex <$> readByte R.LYC,
            cgbPalette "bcps" <$> readByte R.BCPS,
            field "bcpd" formatHex
              <$> liftIO (Port.readDirect (Graphics.portBCPD (Emulator.graphicsState emulatorState))),
            cgbPalette "ocps" <$> readByte R.OCPS,
            field "ocpd" formatHex
              <$> liftIO (Port.readDirect (Graphics.portBCPD (Emulator.graphicsState emulatorState))),
            audio1,
            audio2,
            audio3,
            audio4,
            nr50,
            allBitsOf "nr51" <$> readByte R.NR51,
            nr52,
            nibbles "pcm12" <$> readByte R.PCM12,
            nibbles "pcm34" <$> readByte R.PCM34,
            field "wave0" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x0)),
            field "wave1" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x1)),
            field "wave2" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x2)),
            field "wave3" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x3)),
            field "wave4" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x4)),
            field "wave5" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x5)),
            field "wave6" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x6)),
            field "wave7" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x7)),
            field "wave8" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x8)),
            field "wave9" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0x9)),
            field "waveA" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0xA)),
            field "waveB" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0xB)),
            field "waveC" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0xC)),
            field "waveD" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0xD)),
            field "waveE" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0xE)),
            field "waveF" formatHex <$> liftIO (Port.readDirect (waveTable V.! 0xF))
          ]
    field label decoder = pure . (label .=) . decoder
    rAF = do
      (rA, rF) <- highlow <$> CPU.readR16pp PushPopAF
      pure ["rA" .= rA, "rF" .= rF]
    rPC = do
      pc <- CPU.readPC
      bank <- getBank pc
      let (rPCH, rPCL) = highlow pc
      pure ["rPCH" .= rPCH, "rPCL" .= rPCL, "pcBank" .= formatHex bank]
    rSP = do
      sp <- CPU.readR16 RegSP
      let (rSPH, rSPL) = highlow sp
      pure ["rSPH" .= rSPH, "rSPL" .= rSPL, "sp" .= sp]
    flags = do
      i <- CPU.testIME <&> \ime -> if ime then 'I' else 'i'
      r <- CPU.readF
      currentMode <-
        CPU.getMode <&> \case
          CPU.ModeHalt -> ("HALT" :: String)
          CPU.ModeStop -> "STOP"
          CPU.ModeNormal -> "RUN"
      pure
        [ "i" .= i,
          "z" .= if isFlagSet CPU.flagZ r then 'Z' else 'z',
          "n" .= if isFlagSet CPU.flagN r then 'N' else 'n',
          "h" .= if isFlagSet CPU.flagH r then 'H' else 'h',
          "c" .= if isFlagSet CPU.flagCY r then 'C' else 'c',
          "cpuMode" .= currentMode
        ]
    p1 = do
      r <- readByte R.P1
      pure
        [ "p15" .= getBit 5 r,
          "p14" .= getBit 4 r,
          "p13" .= getBit 3 r,
          "p12" .= getBit 2 r,
          "p11" .= getBit 1 r,
          "p10" .= getBit 0 r
        ]
    tac = do
      r <- readByte R.TAC
      pure ["tac2" .= getBit 2 r, "tac1_0" .= formatHex (r .&. 3) !! 1]
    key1 = do
      r <- readByte R.KEY1
      pure ["key17" .= getBit 7 r, "key10" .= getBit 0 r]
    svbk = do
      r <- readByte R.SVBK
      pure
        [ "svbk7" .= getBit 7 r,
          "svbk5" .= getBit 5 r,
          "svbk4" .= getBit 4 r,
          "svbk3" .= getBit 3 r,
          "svbk2_0" .= formatHex (r .&. 7) !! 1
        ]
    ie = do
      r <- readByte R.IE
      pure
        [ "ie4" .= getBit 4 r,
          "ie3" .= getBit 3 r,
          "ie2" .= getBit 2 r,
          "ie1" .= getBit 1 r,
          "ie0" .= getBit 0 r
        ]
    rif = do
      r <- readByte R.IF
      pure
        [ "if4" .= getBit 4 r,
          "if3" .= getBit 3 r,
          "if2" .= getBit 2 r,
          "if1" .= getBit 1 r,
          "if0" .= getBit 0 r
        ]
    allBitsOf name r =
      [ (name <> "7") .= getBit 7 r,
        (name <> "6") .= getBit 6 r,
        (name <> "5") .= getBit 5 r,
        (name <> "4") .= getBit 4 r,
        (name <> "3") .= getBit 3 r,
        (name <> "2") .= getBit 2 r,
        (name <> "1") .= getBit 1 r,
        (name <> "0") .= getBit 0 r
      ]
    sc = do
      r <- readByte R.SC
      pure ["sc7" .= getBit 7 r, "sc1" .= getBit 1 r, "sc0" .= getBit 0 r]
    rp = do
      r <- readByte R.RP
      pure ["rp7_6" .= formatHex (r .>>. 6) !! 1, "rp1" .= getBit 1 r, "rp0" .= getBit 0 r]
    romBanks = do
      bank0 <- getBank 0x3000
      bank1 <- getBank 0x4000
      pure
        [ "rom0Bank" .= formatHex bank0,
          "rom1Bank" .= formatHex bank1,
          "romBank" .= formatHex (if bank0 == 0 then bank1 else bank0)
        ]
    hdma5 = do
      r <- readByte R.HDMA5
      pure ["hdma57" .= getBit 7 r, "hdma56_0" .= formatHex (r .&. 0x7F)]
    dmgPalette name r =
      [ (name <> "76") .= formatHex (r .>>. 6) !! 1,
        (name <> "54") .= formatHex ((r .>>. 4) .&. 3) !! 1,
        (name <> "32") .= formatHex ((r .>>. 2) .&. 3) !! 1,
        (name <> "10") .= formatHex (r .&. 3) !! 1
      ]
    stat = do
      r <- readByte R.STAT
      pure
        [ "stat6" .= getBit 6 r,
          "stat5" .= getBit 5 r,
          "stat4" .= getBit 4 r,
          "stat3" .= getBit 3 r,
          "stat2" .= getBit 2 r,
          "stat1_0" .= formatHex (r .&. 3) !! 1
        ]
    cgbPalette name r =
      [ (name <> "7") .= getBit 7 r,
        (name <> "5_3") .= formatHex ((r .>>. 3) .&. 7) !! 1,
        (name <> "2_1") .= formatHex ((r .>>. 1) .&. 3) !! 1,
        (name <> "0") .= getBit 0 r
      ]
    waveTable = portWaveTable . Audio.channel3 . Emulator.audioState $ emulatorState
    audio1 = do
      (nr10, nr11, nr12, nr13raw, nr14) <-
        liftIO
          (directReadPorts (Audio.channel1 . Emulator.audioState $ emulatorState))
      pure
        [ "nr13" .= formatHex nr13raw,
          "nr106_4" .= formatHex ((nr10 .>>. 4) .&. 7) !! 1,
          "nr103" .= getBit 3 nr10,
          "nr102_0" .= formatHex (nr10 .&. 7) !! 1,
          "nr117_6" .= formatHex (nr11 .>>. 6) !! 1,
          "nr115_0" .= formatHex (nr11 .&. 0x3F),
          "nr127_4" .= head (formatHex nr12),
          "nr123" .= getBit 3 nr12,
          "nr122_0" .= formatHex (nr12 .&. 7) !! 1,
          "nr147" .= getBit 7 nr14,
          "nr146" .= getBit 6 nr14,
          "nr142_0" .= formatHex (nr14 .&. 7) !! 1
        ]
    audio2 = do
      (_, nr21, nr22, nr23raw, nr24) <-
        liftIO
          (directReadPorts (Audio.channel2 . Emulator.audioState $ emulatorState))
      pure
        [ "nr23" .= formatHex nr23raw,
          "nr217_6" .= formatHex (nr21 .>>. 6) !! 1,
          "nr215_0" .= formatHex (nr21 .&. 0x3F),
          "nr227_4" .= head (formatHex nr22),
          "nr223" .= getBit 3 nr22,
          "nr222_0" .= formatHex (nr22 .&. 7) !! 1,
          "nr247" .= getBit 7 nr24,
          "nr246" .= getBit 6 nr24,
          "nr242_0" .= formatHex (nr24 .&. 7) !! 1
        ]
    audio3 = do
      (nr30, nr31raw, nr32, nr33raw, nr34) <-
        liftIO
          (directReadPorts (Audio.channel3 . Emulator.audioState $ emulatorState))
      pure
        [ "nr31" .= formatHex nr31raw,
          "nr33" .= formatHex nr33raw,
          "nr307" .= getBit 7 nr30,
          "nr326_5" .= formatHex ((nr32 .>>. 5) .&. 3) !! 1,
          "nr347" .= getBit 7 nr34,
          "nr346" .= getBit 6 nr34,
          "nr342_0" .= formatHex (nr34 .&. 7) !! 1
        ]
    audio4 = do
      (_, nr41, nr42, nr43, nr44) <-
        liftIO
          (directReadPorts (Audio.channel4 . Emulator.audioState $ emulatorState))
      pure
        [ "nr415_0" .= formatHex (nr41 .&. 0x3F),
          "nr427_4" .= head (formatHex nr42),
          "nr423" .= getBit 3 nr42,
          "nr422_0" .= formatHex (nr42 .&. 7) !! 1,
          "nr437_4" .= head (formatHex nr43),
          "nr433" .= getBit 3 nr43,
          "nr432_0" .= formatHex (nr43 .&. 7) !! 1,
          "nr447" .= getBit 7 nr44,
          "nr446" .= getBit 6 nr44
        ]
    nr50 = do
      r <- readByte R.NR50
      pure
        [ "nr507" .= getBit 7 r,
          "nr506_4" .= head (formatHex (r .&. 0x70)),
          "nr503" .= getBit 3 r,
          "nr502_0" .= formatHex (r .&. 0x07) !! 1
        ]
    nr52 = do
      r <- readByte R.NR52
      pure
        [ "nr527" .= getBit 7 r,
          "nr523" .= getBit 3 r,
          "nr522" .= getBit 2 r,
          "nr521" .= getBit 1 r,
          "nr520" .= getBit 0 r
        ]
    nibbles name r = [(name <> "7_4") .= head (formatHex r), (name <> "3_0") .= formatHex r !! 1]
