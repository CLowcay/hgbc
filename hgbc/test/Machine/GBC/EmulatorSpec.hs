{-# LANGUAGE OverloadedStrings #-}

module Machine.GBC.EmulatorSpec where

import           Data.List
import           Foreign.Ptr
import           Machine.GBC.Audio
import           Machine.GBC.CPU
import           Machine.GBC.DMA
import           Machine.GBC.Emulator
import           Machine.GBC.Graphics
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Keypad
import           Machine.GBC.ROM
import           Machine.GBC.Registers
import           Machine.GBC.Timer
import           Test.Hspec
import qualified Data.ByteString               as B

blankROM :: ROM
blankROM = ROM paths (blankHeader size) (B.replicate size 0)
 where
  paths = ROMPaths "testRom" "testRom.sav" "testRom.rtc"
  size  = 32 * 1024 * 1024

blankHeader :: Int -> Header
blankHeader romSize = Header { startAddress          = 0
                             , nintendoCharacterData = ""
                             , gameTitle             = ""
                             , gameCode              = ""
                             , cgbSupport            = CGBCompatible
                             , makerCode             = ""
                             , sgbSupport            = GBOnly
                             , cartridgeType         = CartridgeType Nothing False False
                             , romSize               = romSize
                             , externalRAM           = 0
                             , destination           = Overseas
                             , oldLicenseCode        = 0
                             , maskROMVersion        = 0
                             }

spec :: Spec
spec = describe "allPorts" $ it "all hardware ports are accounted for" $ do
  sync     <- newGraphicsSync
  emulator <- initEmulatorState blankROM NoColorCorrection sync nullPtr
  let allPorts =
        cpuPorts (cpu emulator)
          ++ dmaPorts (dmaState emulator)
          ++ graphicsPorts (graphicsState emulator)
          ++ keypadPorts (keypadState emulator)
          ++ timerPorts (timerState emulator)
          ++ audioPorts (audioState emulator)
  nub (fst <$> allPorts) `shouldBe` (fst <$> allPorts)
  sort (fst <$> allPorts) `shouldBe` sort
    (  [0xFF30 .. 0xFF3F]
    ++ [ P1
       , DIV
       , TIMA
       , TMA
       , TAC
       , NR10
       , NR11
       , NR12
       , NR13
       , NR14
       , NR20
       , NR21
       , NR22
       , NR23
       , NR24
       , NR30
       , NR31
       , NR32
       , NR33
       , NR34
       , NR41
       , NR42
       , NR43
       , NR44
       , NR50
       , NR51
       , NR52
       , LCDC
       , STAT
       , SCY
       , SCX
       , LY
       , LYC
       , DMA
       , BGP
       , OBP0
       , OBP1
       , WY
       , WX
       , KEY1
       , VBK
       , HDMA1
       , HDMA2
       , HDMA3
       , HDMA4
       , HDMA5
       , BCPS
       , BCPD
       , OCPS
       , OCPD
       , PCM12
       , PCM34
       ]
    )
