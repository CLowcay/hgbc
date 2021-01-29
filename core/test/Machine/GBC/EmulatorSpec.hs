{-# LANGUAGE OverloadedStrings #-}

module Machine.GBC.EmulatorSpec
  ( spec,
  )
where

import qualified Data.ByteString as B
import Data.List (nub, sort)
import Foreign.Ptr (nullPtr)
import qualified Machine.GBC.Audio as Audio
import qualified Machine.GBC.CPU as CPU
import qualified Machine.GBC.Color as Color
import qualified Machine.GBC.DMA as DMA
import Machine.GBC.Emulator (State (..))
import qualified Machine.GBC.Emulator as Emulator
import qualified Machine.GBC.Graphics as Graphics
import qualified Machine.GBC.Keypad as Keypad
import qualified Machine.GBC.ROM as ROM
import qualified Machine.GBC.Registers as R
import qualified Machine.GBC.Serial as Serial
import qualified Machine.GBC.Timer as Timer
import Test.Hspec (Spec, describe, it, shouldBe)

blankROM :: ROM.ROM
blankROM = ROM.ROM paths (blankHeader size) (B.replicate size 0)
  where
    paths = ROM.Paths "testRom" "testRom.sav" "testRom.rtc"
    size = 32 * 1024 * 1024

blankHeader :: Int -> ROM.Header
blankHeader romSize =
  ROM.Header
    { ROM.startAddress = 0,
      ROM.nintendoCharacterData = "",
      ROM.gameTitle = "",
      ROM.gameCode = "",
      ROM.cgbSupport = ROM.CGBCompatible,
      ROM.makerCode = "",
      ROM.sgbSupport = ROM.GBOnly,
      ROM.cartridgeType = ROM.CartridgeType Nothing False False,
      ROM.romSize = romSize,
      ROM.externalRAM = 0,
      ROM.destination = ROM.Overseas,
      ROM.oldLicenseCode = 0,
      ROM.maskROMVersion = 0
    }

spec :: Spec
spec = describe "allPorts" $
  it "all hardware ports are accounted for" $ do
    sync <- Graphics.newSync
    serialSync <- Serial.newSync
    emulator <-
      Emulator.init
        Nothing
        blankROM
        Nothing
        (Color.correction Color.NoCorrection)
        serialSync
        sync
        nullPtr
    let allPorts =
          CPU.ports (cpu emulator)
            ++ DMA.ports (dmaState emulator)
            ++ Graphics.ports (graphicsState emulator)
            ++ Keypad.ports (keypadState emulator)
            ++ Timer.ports (timerState emulator)
            ++ Audio.ports (audioState emulator)
            ++ Serial.ports (serialState emulator)
    nub (fst <$> allPorts) `shouldBe` (fst <$> allPorts)
    sort (fst <$> allPorts)
      `shouldBe` sort
        ( [0xFF30 .. 0xFF3F]
            ++ [ R.P1,
                 R.SB,
                 R.SC,
                 R.DIV,
                 R.TIMA,
                 R.TMA,
                 R.TAC,
                 R.NR10,
                 R.NR11,
                 R.NR12,
                 R.NR13,
                 R.NR14,
                 R.NR20,
                 R.NR21,
                 R.NR22,
                 R.NR23,
                 R.NR24,
                 R.NR30,
                 R.NR31,
                 R.NR32,
                 R.NR33,
                 R.NR34,
                 R.NR41,
                 R.NR42,
                 R.NR43,
                 R.NR44,
                 R.NR50,
                 R.NR51,
                 R.NR52,
                 R.LCDC,
                 R.STAT,
                 R.SCY,
                 R.SCX,
                 R.LY,
                 R.LYC,
                 R.DMA,
                 R.BGP,
                 R.OBP0,
                 R.OBP1,
                 R.WY,
                 R.WX,
                 R.KEY1,
                 R.VBK,
                 R.HDMA1,
                 R.HDMA2,
                 R.HDMA3,
                 R.HDMA4,
                 R.HDMA5,
                 R.BCPS,
                 R.BCPD,
                 R.OCPS,
                 R.OCPD,
                 R.PCM12,
                 R.PCM34
               ]
        )
