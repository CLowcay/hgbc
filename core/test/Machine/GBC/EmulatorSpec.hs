{-# LANGUAGE OverloadedStrings #-}

module Machine.GBC.EmulatorSpec
  ( spec,
  )
where

import qualified Data.ByteString as B
import Data.List
import Foreign.Ptr
import qualified Machine.GBC.Audio as Audio
import qualified Machine.GBC.CPU as CPU
import qualified Machine.GBC.Color as Color
import qualified Machine.GBC.DMA as DMA
import Machine.GBC.Emulator
import qualified Machine.GBC.Emulator as Emulator
import qualified Machine.GBC.Graphics as Graphics
import qualified Machine.GBC.Keypad as Keypad
import Machine.GBC.ROM
import qualified Machine.GBC.ROM as ROM
import Machine.GBC.Registers
import qualified Machine.GBC.Serial as Serial
import qualified Machine.GBC.Timer as Timer
import Test.Hspec

blankROM :: ROM.ROM
blankROM = ROM.ROM paths (blankHeader size) (B.replicate size 0)
  where
    paths = ROM.Paths "testRom" "testRom.sav" "testRom.rtc"
    size = 32 * 1024 * 1024

blankHeader :: Int -> Header
blankHeader romSize =
  Header
    { startAddress = 0,
      nintendoCharacterData = "",
      gameTitle = "",
      gameCode = "",
      cgbSupport = CGBCompatible,
      makerCode = "",
      sgbSupport = GBOnly,
      cartridgeType = CartridgeType Nothing False False,
      romSize = romSize,
      externalRAM = 0,
      destination = Overseas,
      oldLicenseCode = 0,
      maskROMVersion = 0
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
            ++ [ P1,
                 SB,
                 SC,
                 DIV,
                 TIMA,
                 TMA,
                 TAC,
                 NR10,
                 NR11,
                 NR12,
                 NR13,
                 NR14,
                 NR20,
                 NR21,
                 NR22,
                 NR23,
                 NR24,
                 NR30,
                 NR31,
                 NR32,
                 NR33,
                 NR34,
                 NR41,
                 NR42,
                 NR43,
                 NR44,
                 NR50,
                 NR51,
                 NR52,
                 LCDC,
                 STAT,
                 SCY,
                 SCX,
                 LY,
                 LYC,
                 DMA,
                 BGP,
                 OBP0,
                 OBP1,
                 WY,
                 WX,
                 KEY1,
                 VBK,
                 HDMA1,
                 HDMA2,
                 HDMA3,
                 HDMA4,
                 HDMA5,
                 BCPS,
                 BCPD,
                 OCPS,
                 OCPD,
                 PCM12,
                 PCM34
               ]
        )
