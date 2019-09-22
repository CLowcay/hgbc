{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
module GBC.DecodeSpec where

import           Control.Monad.Reader
import           Data.Word
import           GBC.Decode
import           GBC.ISA
import           GBC.Memory
import           GBC.ROM
import           Test.Hspec
import qualified Data.ByteString               as B

makeROM :: [Word8] -> ROM
makeROM d = ROM $ B.take (32 * 1024 * 1024) $ B.pack (0xFF : d) <> B.replicate (32 * 1024 * 1024) 0

decodesTo :: [Word8] -> Instruction -> IO ()
decodesTo encoding expectedDecoding = do
  memory               <- initMemory $ makeROM encoding
  (instruction, addr1) <- runReaderT (runDecode 1 decode) memory
  instruction `shouldBe` expectedDecoding
  addr1 `shouldBe` 1 + fromIntegral (length encoding)

spec :: Spec
spec = describe "decode" $ do
  it "decodes LD B B" $ [0b01_000_000] `decodesTo` LD_R8 RegB (R8 RegB)
  it "decodes LD C B" $ [0b01_001_000] `decodesTo` LD_R8 RegC (R8 RegB)
  it "decodes LD D B" $ [0b01_010_000] `decodesTo` LD_R8 RegD (R8 RegB)
  it "decodes LD E B" $ [0b01_011_000] `decodesTo` LD_R8 RegE (R8 RegB)
  it "decodes LD H B" $ [0b01_100_000] `decodesTo` LD_R8 RegH (R8 RegB)
  it "decodes LD L B" $ [0b01_101_000] `decodesTo` LD_R8 RegL (R8 RegB)
  it "decodes LD A B" $ [0b01_111_000] `decodesTo` LD_R8 RegA (R8 RegB)
  it "decodes LD B C" $ [0b01_000_001] `decodesTo` LD_R8 RegB (R8 RegC)
  it "decodes LD C C" $ [0b01_001_001] `decodesTo` LD_R8 RegC (R8 RegC)
  it "decodes LD D C" $ [0b01_010_001] `decodesTo` LD_R8 RegD (R8 RegC)
  it "decodes LD E C" $ [0b01_011_001] `decodesTo` LD_R8 RegE (R8 RegC)
  it "decodes LD H C" $ [0b01_100_001] `decodesTo` LD_R8 RegH (R8 RegC)
  it "decodes LD L C" $ [0b01_101_001] `decodesTo` LD_R8 RegL (R8 RegC)
  it "decodes LD A C" $ [0b01_111_001] `decodesTo` LD_R8 RegA (R8 RegC)
  it "decodes LD B D" $ [0b01_000_010] `decodesTo` LD_R8 RegB (R8 RegD)
  it "decodes LD C D" $ [0b01_001_010] `decodesTo` LD_R8 RegC (R8 RegD)
  it "decodes LD D D" $ [0b01_010_010] `decodesTo` LD_R8 RegD (R8 RegD)
  it "decodes LD E D" $ [0b01_011_010] `decodesTo` LD_R8 RegE (R8 RegD)
  it "decodes LD H D" $ [0b01_100_010] `decodesTo` LD_R8 RegH (R8 RegD)
  it "decodes LD L D" $ [0b01_101_010] `decodesTo` LD_R8 RegL (R8 RegD)
  it "decodes LD A D" $ [0b01_111_010] `decodesTo` LD_R8 RegA (R8 RegD)
  it "decodes LD B E" $ [0b01_000_011] `decodesTo` LD_R8 RegB (R8 RegE)
  it "decodes LD C E" $ [0b01_001_011] `decodesTo` LD_R8 RegC (R8 RegE)
  it "decodes LD D E" $ [0b01_010_011] `decodesTo` LD_R8 RegD (R8 RegE)
  it "decodes LD E E" $ [0b01_011_011] `decodesTo` LD_R8 RegE (R8 RegE)
  it "decodes LD H E" $ [0b01_100_011] `decodesTo` LD_R8 RegH (R8 RegE)
  it "decodes LD L E" $ [0b01_101_011] `decodesTo` LD_R8 RegL (R8 RegE)
  it "decodes LD A E" $ [0b01_111_011] `decodesTo` LD_R8 RegA (R8 RegE)
  it "decodes LD B H" $ [0b01_000_100] `decodesTo` LD_R8 RegB (R8 RegH)
  it "decodes LD C H" $ [0b01_001_100] `decodesTo` LD_R8 RegC (R8 RegH)
  it "decodes LD D H" $ [0b01_010_100] `decodesTo` LD_R8 RegD (R8 RegH)
  it "decodes LD E H" $ [0b01_011_100] `decodesTo` LD_R8 RegE (R8 RegH)
  it "decodes LD H H" $ [0b01_100_100] `decodesTo` LD_R8 RegH (R8 RegH)
  it "decodes LD L H" $ [0b01_101_100] `decodesTo` LD_R8 RegL (R8 RegH)
  it "decodes LD A H" $ [0b01_111_100] `decodesTo` LD_R8 RegA (R8 RegH)
  it "decodes LD B L" $ [0b01_000_101] `decodesTo` LD_R8 RegB (R8 RegL)
  it "decodes LD C L" $ [0b01_001_101] `decodesTo` LD_R8 RegC (R8 RegL)
  it "decodes LD D L" $ [0b01_010_101] `decodesTo` LD_R8 RegD (R8 RegL)
  it "decodes LD E L" $ [0b01_011_101] `decodesTo` LD_R8 RegE (R8 RegL)
  it "decodes LD H L" $ [0b01_100_101] `decodesTo` LD_R8 RegH (R8 RegL)
  it "decodes LD L L" $ [0b01_101_101] `decodesTo` LD_R8 RegL (R8 RegL)
  it "decodes LD A L" $ [0b01_111_101] `decodesTo` LD_R8 RegA (R8 RegL)
  it "decodes LD B n" $ [0b00_000_110, 0x42] `decodesTo` LD_R8 RegB (I8 0x42)
  it "decodes LD C n" $ [0b00_001_110, 0x42] `decodesTo` LD_R8 RegC (I8 0x42)
  it "decodes LD D n" $ [0b00_010_110, 0x42] `decodesTo` LD_R8 RegD (I8 0x42)
  it "decodes LD E n" $ [0b00_011_110, 0x42] `decodesTo` LD_R8 RegE (I8 0x42)
  it "decodes LD H n" $ [0b00_100_110, 0x42] `decodesTo` LD_R8 RegH (I8 0x42)
  it "decodes LD L n" $ [0b00_101_110, 0x42] `decodesTo` LD_R8 RegL (I8 0x42)
  it "decodes LD A n" $ [0b00_111_110, 0x42] `decodesTo` LD_R8 RegA (I8 0x42)
  it "decodes LD B (HL)" $ [0b01_000_110] `decodesTo` LD_R8 RegB HLI
  it "decodes LD C (HL)" $ [0b01_001_110] `decodesTo` LD_R8 RegC HLI
  it "decodes LD D (HL)" $ [0b01_010_110] `decodesTo` LD_R8 RegD HLI
  it "decodes LD E (HL)" $ [0b01_011_110] `decodesTo` LD_R8 RegE HLI
  it "decodes LD H (HL)" $ [0b01_100_110] `decodesTo` LD_R8 RegH HLI
  it "decodes LD L (HL)" $ [0b01_101_110] `decodesTo` LD_R8 RegL HLI
  it "decodes LD A (HL)" $ [0b01_111_110] `decodesTo` LD_R8 RegA HLI
  it "decodes LD (HL) B" $ [0b01_110_000] `decodesTo` LDHLI_R8 RegB
  it "decodes LD (HL) C" $ [0b01_110_001] `decodesTo` LDHLI_R8 RegC
  it "decodes LD (HL) D" $ [0b01_110_010] `decodesTo` LDHLI_R8 RegD
  it "decodes LD (HL) E" $ [0b01_110_011] `decodesTo` LDHLI_R8 RegE
  it "decodes LD (HL) H" $ [0b01_110_100] `decodesTo` LDHLI_R8 RegH
  it "decodes LD (HL) L" $ [0b01_110_101] `decodesTo` LDHLI_R8 RegL
  it "decodes LD (HL) A" $ [0b01_110_111] `decodesTo` LDHLI_R8 RegA
  it "decodes LD (HL) n" $ [0b00_110_110, 0x42] `decodesTo` LDHLI_I8 0x42
  it "decodes LD A (BC)" $ [0b00_001_010] `decodesTo` LDA_BCI
  it "decodes LD A (DE)" $ [0b00_011_010] `decodesTo` LDA_DEI
  it "decodes LD A (C)" $ [0b11_110_010] `decodesTo` LDA_CI
  it "decodes LD (C) A" $ [0b11_100_010] `decodesTo` LDCI_A
  it "decodes LD A (n)" $ [0b11_110_000, 0x42] `decodesTo` LDA_I8I 0x42
  it "decodes LD (n) A" $ [0b11_100_000, 0x42] `decodesTo` LDI8I_A 0x42
  it "decodes LD A (nn)" $ [0b11_111_010, 0x42, 0x32] `decodesTo` LDA_I16I 0x3242
  it "decodes LD (nn) A" $ [0b11_101_010, 0x42, 0x32] `decodesTo` LDI16I_A 0x3242
  it "decodes LD A (HLI)" $ [0b00_101_010] `decodesTo` LDA_INC
  it "decodes LD A (HLD)" $ [0b00_111_010] `decodesTo` LDA_DEC
  it "decodes LD (BC) A" $ [0b00_000_010] `decodesTo` LDBCI_A
  it "decodes LD (DE) A" $ [0b00_010_010] `decodesTo` LDDEI_A
  it "decodes LD (HLI) A" $ [0b00_100_010] `decodesTo` LDHLI_INC
  it "decodes LD (HLD) A" $ [0b00_110_010] `decodesTo` LDHLI_DEC
  it "decodes LD BC n" $ [0b00_000_001, 0x42, 0x32] `decodesTo` LD16_I16 RegBC 0x3242
  it "decodes LD DE n" $ [0b00_010_001, 0x42, 0x32] `decodesTo` LD16_I16 RegDE 0x3242
  it "decodes LD HL n" $ [0b00_100_001, 0x42, 0x32] `decodesTo` LD16_I16 RegHL 0x3242
  it "decodes LD SP n" $ [0b00_110_001, 0x42, 0x32] `decodesTo` LD16_I16 RegSP 0x3242
  it "decodes LD SP HL" $ [0b11_111_001] `decodesTo` LDSP
  it "decodes PUSH BC" $ [0b11_000_101] `decodesTo` PUSH RegBC
  it "decodes PUSH DE" $ [0b11_010_101] `decodesTo` PUSH RegDE
  it "decodes PUSH HL" $ [0b11_100_101] `decodesTo` PUSH RegHL
  it "decodes PUSH AF" $ [0b11_110_101] `decodesTo` PUSH RegSP
  it "decodes POP BC" $ [0b11_000_001] `decodesTo` POP RegBC
  it "decodes POP DE" $ [0b11_010_001] `decodesTo` POP RegDE
  it "decodes POP HL" $ [0b11_100_001] `decodesTo` POP RegHL
  it "decodes POP AF" $ [0b11_110_001] `decodesTo` POP RegSP
  it "decodes LDHL SP e" $ [0b11_111_000, 0x42] `decodesTo` LDHL 0x42
  it "decodes LD (n) SP" $ [0b00_001_000, 0x42, 0x32] `decodesTo` LDI16I_SP 0x3242
  it "decodes ADD A B" $ [0b10_000_000] `decodesTo` ADD (R8 RegB)
  it "decodes ADD A C" $ [0b10_000_001] `decodesTo` ADD (R8 RegC)
  it "decodes ADD A D" $ [0b10_000_010] `decodesTo` ADD (R8 RegD)
  it "decodes ADD A E" $ [0b10_000_011] `decodesTo` ADD (R8 RegE)
  it "decodes ADD A H" $ [0b10_000_100] `decodesTo` ADD (R8 RegH)
  it "decodes ADD A L" $ [0b10_000_101] `decodesTo` ADD (R8 RegL)
  it "decodes ADD A A" $ [0b10_000_111] `decodesTo` ADD (R8 RegA)
  it "decodes ADD A n" $ [0b11_000_110, 0x42] `decodesTo` ADD (I8 0x42)
  it "decodes ADD A (HL)" $ [0b10_000_110] `decodesTo` ADD HLI
  it "decodes ADC A B" $ [0b10_001_000] `decodesTo` ADC (R8 RegB)
  it "decodes ADC A C" $ [0b10_001_001] `decodesTo` ADC (R8 RegC)
  it "decodes ADC A D" $ [0b10_001_010] `decodesTo` ADC (R8 RegD)
  it "decodes ADC A E" $ [0b10_001_011] `decodesTo` ADC (R8 RegE)
  it "decodes ADC A H" $ [0b10_001_100] `decodesTo` ADC (R8 RegH)
  it "decodes ADC A L" $ [0b10_001_101] `decodesTo` ADC (R8 RegL)
  it "decodes ADC A A" $ [0b10_001_111] `decodesTo` ADC (R8 RegA)
  it "decodes ADC A n" $ [0b11_001_110, 0x42] `decodesTo` ADC (I8 0x42)
  it "decodes ADC A (HL)" $ [0b10_001_110] `decodesTo` ADC HLI
  it "decodes SUB A B" $ [0b10_010_000] `decodesTo` SUB (R8 RegB)
  it "decodes SUB A C" $ [0b10_010_001] `decodesTo` SUB (R8 RegC)
  it "decodes SUB A D" $ [0b10_010_010] `decodesTo` SUB (R8 RegD)
  it "decodes SUB A E" $ [0b10_010_011] `decodesTo` SUB (R8 RegE)
  it "decodes SUB A H" $ [0b10_010_100] `decodesTo` SUB (R8 RegH)
  it "decodes SUB A L" $ [0b10_010_101] `decodesTo` SUB (R8 RegL)
  it "decodes SUB A A" $ [0b10_010_111] `decodesTo` SUB (R8 RegA)
  it "decodes SUB A n" $ [0b11_010_110, 0x42] `decodesTo` SUB (I8 0x42)
  it "decodes SUB A (HL)" $ [0b10_010_110] `decodesTo` SUB HLI
  it "decodes SBC A B" $ [0b10_011_000] `decodesTo` SBC (R8 RegB)
  it "decodes SBC A C" $ [0b10_011_001] `decodesTo` SBC (R8 RegC)
  it "decodes SBC A D" $ [0b10_011_010] `decodesTo` SBC (R8 RegD)
  it "decodes SBC A E" $ [0b10_011_011] `decodesTo` SBC (R8 RegE)
  it "decodes SBC A H" $ [0b10_011_100] `decodesTo` SBC (R8 RegH)
  it "decodes SBC A L" $ [0b10_011_101] `decodesTo` SBC (R8 RegL)
  it "decodes SBC A A" $ [0b10_011_111] `decodesTo` SBC (R8 RegA)
  it "decodes SBC A n" $ [0b11_011_110, 0x42] `decodesTo` SBC (I8 0x42)
  it "decodes SBC A (HL)" $ [0b10_011_110] `decodesTo` SBC HLI
  it "decodes AND A B" $ [0b10_100_000] `decodesTo` AND (R8 RegB)
  it "decodes AND A C" $ [0b10_100_001] `decodesTo` AND (R8 RegC)
  it "decodes AND A D" $ [0b10_100_010] `decodesTo` AND (R8 RegD)
  it "decodes AND A E" $ [0b10_100_011] `decodesTo` AND (R8 RegE)
  it "decodes AND A H" $ [0b10_100_100] `decodesTo` AND (R8 RegH)
  it "decodes AND A L" $ [0b10_100_101] `decodesTo` AND (R8 RegL)
  it "decodes AND A A" $ [0b10_100_111] `decodesTo` AND (R8 RegA)
  it "decodes AND A n" $ [0b11_100_110, 0x42] `decodesTo` AND (I8 0x42)
  it "decodes AND A (HL)" $ [0b10_100_110] `decodesTo` AND HLI
  it "decodes XOR A B" $ [0b10_101_000] `decodesTo` XOR (R8 RegB)
  it "decodes XOR A C" $ [0b10_101_001] `decodesTo` XOR (R8 RegC)
  it "decodes XOR A D" $ [0b10_101_010] `decodesTo` XOR (R8 RegD)
  it "decodes XOR A E" $ [0b10_101_011] `decodesTo` XOR (R8 RegE)
  it "decodes XOR A H" $ [0b10_101_100] `decodesTo` XOR (R8 RegH)
  it "decodes XOR A L" $ [0b10_101_101] `decodesTo` XOR (R8 RegL)
  it "decodes XOR A A" $ [0b10_101_111] `decodesTo` XOR (R8 RegA)
  it "decodes XOR A n" $ [0b11_101_110, 0x42] `decodesTo` XOR (I8 0x42)
  it "decodes XOR A (HL)" $ [0b10_101_110] `decodesTo` XOR HLI
  it "decodes OR A B" $ [0b10_110_000] `decodesTo` OR (R8 RegB)
  it "decodes OR A C" $ [0b10_110_001] `decodesTo` OR (R8 RegC)
  it "decodes OR A D" $ [0b10_110_010] `decodesTo` OR (R8 RegD)
  it "decodes OR A E" $ [0b10_110_011] `decodesTo` OR (R8 RegE)
  it "decodes OR A H" $ [0b10_110_100] `decodesTo` OR (R8 RegH)
  it "decodes OR A L" $ [0b10_110_101] `decodesTo` OR (R8 RegL)
  it "decodes OR A A" $ [0b10_110_111] `decodesTo` OR (R8 RegA)
  it "decodes OR A n" $ [0b11_110_110, 0x42] `decodesTo` OR (I8 0x42)
  it "decodes OR A (HL)" $ [0b10_110_110] `decodesTo` OR HLI
  it "decodes CP A B" $ [0b10_111_000] `decodesTo` CP (R8 RegB)
  it "decodes CP A C" $ [0b10_111_001] `decodesTo` CP (R8 RegC)
  it "decodes CP A D" $ [0b10_111_010] `decodesTo` CP (R8 RegD)
  it "decodes CP A E" $ [0b10_111_011] `decodesTo` CP (R8 RegE)
  it "decodes CP A H" $ [0b10_111_100] `decodesTo` CP (R8 RegH)
  it "decodes CP A L" $ [0b10_111_101] `decodesTo` CP (R8 RegL)
  it "decodes CP A A" $ [0b10_111_111] `decodesTo` CP (R8 RegA)
  it "decodes CP A n" $ [0b11_111_110, 0x42] `decodesTo` CP (I8 0x42)
  it "decodes CP A (HL)" $ [0b10_111_110] `decodesTo` CP HLI
  it "decodes INC B" $ [0b00_000_100] `decodesTo` INC (SmallR8 RegB)
  it "decodes INC C" $ [0b00_001_100] `decodesTo` INC (SmallR8 RegC)
  it "decodes INC D" $ [0b00_010_100] `decodesTo` INC (SmallR8 RegD)
  it "decodes INC E" $ [0b00_011_100] `decodesTo` INC (SmallR8 RegE)
  it "decodes INC H" $ [0b00_100_100] `decodesTo` INC (SmallR8 RegH)
  it "decodes INC L" $ [0b00_101_100] `decodesTo` INC (SmallR8 RegL)
  it "decodes INC A" $ [0b00_111_100] `decodesTo` INC (SmallR8 RegA)
  it "decodes INC (HL)" $ [0b00_110_100] `decodesTo` INC SmallHLI
  it "decodes DEC B" $ [0b00_000_101] `decodesTo` DEC (SmallR8 RegB)
  it "decodes DEC C" $ [0b00_001_101] `decodesTo` DEC (SmallR8 RegC)
  it "decodes DEC D" $ [0b00_010_101] `decodesTo` DEC (SmallR8 RegD)
  it "decodes DEC E" $ [0b00_011_101] `decodesTo` DEC (SmallR8 RegE)
  it "decodes DEC H" $ [0b00_100_101] `decodesTo` DEC (SmallR8 RegH)
  it "decodes DEC L" $ [0b00_101_101] `decodesTo` DEC (SmallR8 RegL)
  it "decodes DEC A" $ [0b00_111_101] `decodesTo` DEC (SmallR8 RegA)
  it "decodes DEC (HL)" $ [0b00_110_101] `decodesTo` DEC SmallHLI
  it "decodes ADD HL BC" $ [0b00_001_001] `decodesTo` ADDHL RegBC
  it "decodes ADD HL DE" $ [0b00_011_001] `decodesTo` ADDHL RegDE
  it "decodes ADD HL HL" $ [0b00_101_001] `decodesTo` ADDHL RegHL
  it "decodes ADD HL SP" $ [0b00_111_001] `decodesTo` ADDHL RegSP
  it "decodes ADD SP e" $ [0b11_101_000, 0x42] `decodesTo` ADDSP 0x42
  it "decodes INC BC" $ [0b00_000_011] `decodesTo` INC16 RegBC
  it "decodes INC DE" $ [0b00_010_011] `decodesTo` INC16 RegDE
  it "decodes INC HL" $ [0b00_100_011] `decodesTo` INC16 RegHL
  it "decodes INC SP" $ [0b00_110_011] `decodesTo` INC16 RegSP
  it "decodes DEC BC" $ [0b00_001_011] `decodesTo` DEC16 RegBC
  it "decodes DEC DE" $ [0b00_011_011] `decodesTo` DEC16 RegDE
  it "decodes DEC HL" $ [0b00_101_011] `decodesTo` DEC16 RegHL
  it "decodes DEC SP" $ [0b00_111_011] `decodesTo` DEC16 RegSP
  it "decodes RLCA" $ [0b00_000_111] `decodesTo` RLCA
  it "decodes RLA" $ [0b00_010_111] `decodesTo` RLA
  it "decodes RRCA" $ [0b00_001_111] `decodesTo` RRCA
  it "decodes RRA" $ [0b00_011_111] `decodesTo` RRA
  it "decodes RLC B" $ [0b11_001_011, 0b00_000_000] `decodesTo` RLC (SmallR8 RegB)
  it "decodes RLC C" $ [0b11_001_011, 0b00_000_001] `decodesTo` RLC (SmallR8 RegC)
  it "decodes RLC D" $ [0b11_001_011, 0b00_000_010] `decodesTo` RLC (SmallR8 RegD)
  it "decodes RLC E" $ [0b11_001_011, 0b00_000_011] `decodesTo` RLC (SmallR8 RegE)
  it "decodes RLC H" $ [0b11_001_011, 0b00_000_100] `decodesTo` RLC (SmallR8 RegH)
  it "decodes RLC L" $ [0b11_001_011, 0b00_000_101] `decodesTo` RLC (SmallR8 RegL)
  it "decodes RLC A" $ [0b11_001_011, 0b00_000_111] `decodesTo` RLC (SmallR8 RegA)
  it "decodes RLC (HL)" $ [0b11_001_011, 0b00_000_110] `decodesTo` RLC SmallHLI
  it "decodes RL B" $ [0b11_001_011, 0b00_010_000] `decodesTo` RL (SmallR8 RegB)
  it "decodes RL C" $ [0b11_001_011, 0b00_010_001] `decodesTo` RL (SmallR8 RegC)
  it "decodes RL D" $ [0b11_001_011, 0b00_010_010] `decodesTo` RL (SmallR8 RegD)
  it "decodes RL E" $ [0b11_001_011, 0b00_010_011] `decodesTo` RL (SmallR8 RegE)
  it "decodes RL H" $ [0b11_001_011, 0b00_010_100] `decodesTo` RL (SmallR8 RegH)
  it "decodes RL L" $ [0b11_001_011, 0b00_010_101] `decodesTo` RL (SmallR8 RegL)
  it "decodes RL A" $ [0b11_001_011, 0b00_010_111] `decodesTo` RL (SmallR8 RegA)
  it "decodes RL (HL)" $ [0b11_001_011, 0b00_010_110] `decodesTo` RL SmallHLI
  it "decodes RRC B" $ [0b11_001_011, 0b00_001_000] `decodesTo` RRC (SmallR8 RegB)
  it "decodes RRC C" $ [0b11_001_011, 0b00_001_001] `decodesTo` RRC (SmallR8 RegC)
  it "decodes RRC D" $ [0b11_001_011, 0b00_001_010] `decodesTo` RRC (SmallR8 RegD)
  it "decodes RRC E" $ [0b11_001_011, 0b00_001_011] `decodesTo` RRC (SmallR8 RegE)
  it "decodes RRC H" $ [0b11_001_011, 0b00_001_100] `decodesTo` RRC (SmallR8 RegH)
  it "decodes RRC L" $ [0b11_001_011, 0b00_001_101] `decodesTo` RRC (SmallR8 RegL)
  it "decodes RRC A" $ [0b11_001_011, 0b00_001_111] `decodesTo` RRC (SmallR8 RegA)
  it "decodes RRC (HL)" $ [0b11_001_011, 0b00_001_110] `decodesTo` RRC SmallHLI
  it "decodes RR B" $ [0b11_001_011, 0b00_011_000] `decodesTo` RR (SmallR8 RegB)
  it "decodes RR C" $ [0b11_001_011, 0b00_011_001] `decodesTo` RR (SmallR8 RegC)
  it "decodes RR D" $ [0b11_001_011, 0b00_011_010] `decodesTo` RR (SmallR8 RegD)
  it "decodes RR E" $ [0b11_001_011, 0b00_011_011] `decodesTo` RR (SmallR8 RegE)
  it "decodes RR H" $ [0b11_001_011, 0b00_011_100] `decodesTo` RR (SmallR8 RegH)
  it "decodes RR L" $ [0b11_001_011, 0b00_011_101] `decodesTo` RR (SmallR8 RegL)
  it "decodes RR A" $ [0b11_001_011, 0b00_011_111] `decodesTo` RR (SmallR8 RegA)
  it "decodes RR (HL)" $ [0b11_001_011, 0b00_011_110] `decodesTo` RR SmallHLI
  it "decodes SLA B" $ [0b11_001_011, 0b00_100_000] `decodesTo` SLA (SmallR8 RegB)
  it "decodes SLA C" $ [0b11_001_011, 0b00_100_001] `decodesTo` SLA (SmallR8 RegC)
  it "decodes SLA D" $ [0b11_001_011, 0b00_100_010] `decodesTo` SLA (SmallR8 RegD)
  it "decodes SLA E" $ [0b11_001_011, 0b00_100_011] `decodesTo` SLA (SmallR8 RegE)
  it "decodes SLA H" $ [0b11_001_011, 0b00_100_100] `decodesTo` SLA (SmallR8 RegH)
  it "decodes SLA L" $ [0b11_001_011, 0b00_100_101] `decodesTo` SLA (SmallR8 RegL)
  it "decodes SLA A" $ [0b11_001_011, 0b00_100_111] `decodesTo` SLA (SmallR8 RegA)
  it "decodes SLA (HL)" $ [0b11_001_011, 0b00_100_110] `decodesTo` SLA SmallHLI
  it "decodes SRA B" $ [0b11_001_011, 0b00_101_000] `decodesTo` SRA (SmallR8 RegB)
  it "decodes SRA C" $ [0b11_001_011, 0b00_101_001] `decodesTo` SRA (SmallR8 RegC)
  it "decodes SRA D" $ [0b11_001_011, 0b00_101_010] `decodesTo` SRA (SmallR8 RegD)
  it "decodes SRA E" $ [0b11_001_011, 0b00_101_011] `decodesTo` SRA (SmallR8 RegE)
  it "decodes SRA H" $ [0b11_001_011, 0b00_101_100] `decodesTo` SRA (SmallR8 RegH)
  it "decodes SRA L" $ [0b11_001_011, 0b00_101_101] `decodesTo` SRA (SmallR8 RegL)
  it "decodes SRA A" $ [0b11_001_011, 0b00_101_111] `decodesTo` SRA (SmallR8 RegA)
  it "decodes SRA (HL)" $ [0b11_001_011, 0b00_101_110] `decodesTo` SRA SmallHLI
  it "decodes SRL B" $ [0b11_001_011, 0b00_111_000] `decodesTo` SRL (SmallR8 RegB)
  it "decodes SRL C" $ [0b11_001_011, 0b00_111_001] `decodesTo` SRL (SmallR8 RegC)
  it "decodes SRL D" $ [0b11_001_011, 0b00_111_010] `decodesTo` SRL (SmallR8 RegD)
  it "decodes SRL E" $ [0b11_001_011, 0b00_111_011] `decodesTo` SRL (SmallR8 RegE)
  it "decodes SRL H" $ [0b11_001_011, 0b00_111_100] `decodesTo` SRL (SmallR8 RegH)
  it "decodes SRL L" $ [0b11_001_011, 0b00_111_101] `decodesTo` SRL (SmallR8 RegL)
  it "decodes SRL A" $ [0b11_001_011, 0b00_111_111] `decodesTo` SRL (SmallR8 RegA)
  it "decodes SRL (HL)" $ [0b11_001_011, 0b00_111_110] `decodesTo` SRL SmallHLI
  it "decodes SWAP B" $ [0b11_001_011, 0b00_110_000] `decodesTo` SWAP (SmallR8 RegB)
  it "decodes SWAP C" $ [0b11_001_011, 0b00_110_001] `decodesTo` SWAP (SmallR8 RegC)
  it "decodes SWAP D" $ [0b11_001_011, 0b00_110_010] `decodesTo` SWAP (SmallR8 RegD)
  it "decodes SWAP E" $ [0b11_001_011, 0b00_110_011] `decodesTo` SWAP (SmallR8 RegE)
  it "decodes SWAP H" $ [0b11_001_011, 0b00_110_100] `decodesTo` SWAP (SmallR8 RegH)
  it "decodes SWAP L" $ [0b11_001_011, 0b00_110_101] `decodesTo` SWAP (SmallR8 RegL)
  it "decodes SWAP A" $ [0b11_001_011, 0b00_110_111] `decodesTo` SWAP (SmallR8 RegA)
  it "decodes SWAP (HL)" $ [0b11_001_011, 0b00_110_110] `decodesTo` SWAP SmallHLI
  it "decodes BIT 0 B" $ [0b11_001_011, 0b01_000_000] `decodesTo` BIT 0 (SmallR8 RegB)
  it "decodes BIT 0 C" $ [0b11_001_011, 0b01_000_001] `decodesTo` BIT 0 (SmallR8 RegC)
  it "decodes BIT 0 D" $ [0b11_001_011, 0b01_000_010] `decodesTo` BIT 0 (SmallR8 RegD)
  it "decodes BIT 0 E" $ [0b11_001_011, 0b01_000_011] `decodesTo` BIT 0 (SmallR8 RegE)
  it "decodes BIT 0 H" $ [0b11_001_011, 0b01_000_100] `decodesTo` BIT 0 (SmallR8 RegH)
  it "decodes BIT 0 L" $ [0b11_001_011, 0b01_000_101] `decodesTo` BIT 0 (SmallR8 RegL)
  it "decodes BIT 0 A" $ [0b11_001_011, 0b01_000_111] `decodesTo` BIT 0 (SmallR8 RegA)
  it "decodes BIT 0 (HL)" $ [0b11_001_011, 0b01_000_110] `decodesTo` BIT 0 SmallHLI
  it "decodes BIT 1 B" $ [0b11_001_011, 0b01_001_000] `decodesTo` BIT 1 (SmallR8 RegB)
  it "decodes BIT 1 C" $ [0b11_001_011, 0b01_001_001] `decodesTo` BIT 1 (SmallR8 RegC)
  it "decodes BIT 1 D" $ [0b11_001_011, 0b01_001_010] `decodesTo` BIT 1 (SmallR8 RegD)
  it "decodes BIT 1 E" $ [0b11_001_011, 0b01_001_011] `decodesTo` BIT 1 (SmallR8 RegE)
  it "decodes BIT 1 H" $ [0b11_001_011, 0b01_001_100] `decodesTo` BIT 1 (SmallR8 RegH)
  it "decodes BIT 1 L" $ [0b11_001_011, 0b01_001_101] `decodesTo` BIT 1 (SmallR8 RegL)
  it "decodes BIT 1 A" $ [0b11_001_011, 0b01_001_111] `decodesTo` BIT 1 (SmallR8 RegA)
  it "decodes BIT 1 (HL)" $ [0b11_001_011, 0b01_001_110] `decodesTo` BIT 1 SmallHLI
  it "decodes BIT 2 B" $ [0b11_001_011, 0b01_010_000] `decodesTo` BIT 2 (SmallR8 RegB)
  it "decodes BIT 2 C" $ [0b11_001_011, 0b01_010_001] `decodesTo` BIT 2 (SmallR8 RegC)
  it "decodes BIT 2 D" $ [0b11_001_011, 0b01_010_010] `decodesTo` BIT 2 (SmallR8 RegD)
  it "decodes BIT 2 E" $ [0b11_001_011, 0b01_010_011] `decodesTo` BIT 2 (SmallR8 RegE)
  it "decodes BIT 2 H" $ [0b11_001_011, 0b01_010_100] `decodesTo` BIT 2 (SmallR8 RegH)
  it "decodes BIT 2 L" $ [0b11_001_011, 0b01_010_101] `decodesTo` BIT 2 (SmallR8 RegL)
  it "decodes BIT 2 A" $ [0b11_001_011, 0b01_010_111] `decodesTo` BIT 2 (SmallR8 RegA)
  it "decodes BIT 2 (HL)" $ [0b11_001_011, 0b01_010_110] `decodesTo` BIT 2 SmallHLI
  it "decodes BIT 3 B" $ [0b11_001_011, 0b01_011_000] `decodesTo` BIT 3 (SmallR8 RegB)
  it "decodes BIT 3 C" $ [0b11_001_011, 0b01_011_001] `decodesTo` BIT 3 (SmallR8 RegC)
  it "decodes BIT 3 D" $ [0b11_001_011, 0b01_011_010] `decodesTo` BIT 3 (SmallR8 RegD)
  it "decodes BIT 3 E" $ [0b11_001_011, 0b01_011_011] `decodesTo` BIT 3 (SmallR8 RegE)
  it "decodes BIT 3 H" $ [0b11_001_011, 0b01_011_100] `decodesTo` BIT 3 (SmallR8 RegH)
  it "decodes BIT 3 L" $ [0b11_001_011, 0b01_011_101] `decodesTo` BIT 3 (SmallR8 RegL)
  it "decodes BIT 3 A" $ [0b11_001_011, 0b01_011_111] `decodesTo` BIT 3 (SmallR8 RegA)
  it "decodes BIT 3 (HL)" $ [0b11_001_011, 0b01_011_110] `decodesTo` BIT 3 SmallHLI
  it "decodes BIT 4 B" $ [0b11_001_011, 0b01_100_000] `decodesTo` BIT 4 (SmallR8 RegB)
  it "decodes BIT 4 C" $ [0b11_001_011, 0b01_100_001] `decodesTo` BIT 4 (SmallR8 RegC)
  it "decodes BIT 4 D" $ [0b11_001_011, 0b01_100_010] `decodesTo` BIT 4 (SmallR8 RegD)
  it "decodes BIT 4 E" $ [0b11_001_011, 0b01_100_011] `decodesTo` BIT 4 (SmallR8 RegE)
  it "decodes BIT 4 H" $ [0b11_001_011, 0b01_100_100] `decodesTo` BIT 4 (SmallR8 RegH)
  it "decodes BIT 4 L" $ [0b11_001_011, 0b01_100_101] `decodesTo` BIT 4 (SmallR8 RegL)
  it "decodes BIT 4 A" $ [0b11_001_011, 0b01_100_111] `decodesTo` BIT 4 (SmallR8 RegA)
  it "decodes BIT 4 (HL)" $ [0b11_001_011, 0b01_100_110] `decodesTo` BIT 4 SmallHLI
  it "decodes BIT 5 B" $ [0b11_001_011, 0b01_101_000] `decodesTo` BIT 5 (SmallR8 RegB)
  it "decodes BIT 5 C" $ [0b11_001_011, 0b01_101_001] `decodesTo` BIT 5 (SmallR8 RegC)
  it "decodes BIT 5 D" $ [0b11_001_011, 0b01_101_010] `decodesTo` BIT 5 (SmallR8 RegD)
  it "decodes BIT 5 E" $ [0b11_001_011, 0b01_101_011] `decodesTo` BIT 5 (SmallR8 RegE)
  it "decodes BIT 5 H" $ [0b11_001_011, 0b01_101_100] `decodesTo` BIT 5 (SmallR8 RegH)
  it "decodes BIT 5 L" $ [0b11_001_011, 0b01_101_101] `decodesTo` BIT 5 (SmallR8 RegL)
  it "decodes BIT 5 A" $ [0b11_001_011, 0b01_101_111] `decodesTo` BIT 5 (SmallR8 RegA)
  it "decodes BIT 5 (HL)" $ [0b11_001_011, 0b01_101_110] `decodesTo` BIT 5 SmallHLI
  it "decodes BIT 6 B" $ [0b11_001_011, 0b01_110_000] `decodesTo` BIT 6 (SmallR8 RegB)
  it "decodes BIT 6 C" $ [0b11_001_011, 0b01_110_001] `decodesTo` BIT 6 (SmallR8 RegC)
  it "decodes BIT 6 D" $ [0b11_001_011, 0b01_110_010] `decodesTo` BIT 6 (SmallR8 RegD)
  it "decodes BIT 6 E" $ [0b11_001_011, 0b01_110_011] `decodesTo` BIT 6 (SmallR8 RegE)
  it "decodes BIT 6 H" $ [0b11_001_011, 0b01_110_100] `decodesTo` BIT 6 (SmallR8 RegH)
  it "decodes BIT 6 L" $ [0b11_001_011, 0b01_110_101] `decodesTo` BIT 6 (SmallR8 RegL)
  it "decodes BIT 6 A" $ [0b11_001_011, 0b01_110_111] `decodesTo` BIT 6 (SmallR8 RegA)
  it "decodes BIT 6 (HL)" $ [0b11_001_011, 0b01_110_110] `decodesTo` BIT 6 SmallHLI
  it "decodes BIT 7 B" $ [0b11_001_011, 0b01_111_000] `decodesTo` BIT 7 (SmallR8 RegB)
  it "decodes BIT 7 C" $ [0b11_001_011, 0b01_111_001] `decodesTo` BIT 7 (SmallR8 RegC)
  it "decodes BIT 7 D" $ [0b11_001_011, 0b01_111_010] `decodesTo` BIT 7 (SmallR8 RegD)
  it "decodes BIT 7 E" $ [0b11_001_011, 0b01_111_011] `decodesTo` BIT 7 (SmallR8 RegE)
  it "decodes BIT 7 H" $ [0b11_001_011, 0b01_111_100] `decodesTo` BIT 7 (SmallR8 RegH)
  it "decodes BIT 7 L" $ [0b11_001_011, 0b01_111_101] `decodesTo` BIT 7 (SmallR8 RegL)
  it "decodes BIT 7 A" $ [0b11_001_011, 0b01_111_111] `decodesTo` BIT 7 (SmallR8 RegA)
  it "decodes BIT 7 (HL)" $ [0b11_001_011, 0b01_111_110] `decodesTo` BIT 7 SmallHLI
  it "decodes SET 0 B" $ [0b11_001_011, 0b11_000_000] `decodesTo` SET 0 (SmallR8 RegB)
  it "decodes SET 0 C" $ [0b11_001_011, 0b11_000_001] `decodesTo` SET 0 (SmallR8 RegC)
  it "decodes SET 0 D" $ [0b11_001_011, 0b11_000_010] `decodesTo` SET 0 (SmallR8 RegD)
  it "decodes SET 0 E" $ [0b11_001_011, 0b11_000_011] `decodesTo` SET 0 (SmallR8 RegE)
  it "decodes SET 0 H" $ [0b11_001_011, 0b11_000_100] `decodesTo` SET 0 (SmallR8 RegH)
  it "decodes SET 0 L" $ [0b11_001_011, 0b11_000_101] `decodesTo` SET 0 (SmallR8 RegL)
  it "decodes SET 0 A" $ [0b11_001_011, 0b11_000_111] `decodesTo` SET 0 (SmallR8 RegA)
  it "decodes SET 0 (HL)" $ [0b11_001_011, 0b11_000_110] `decodesTo` SET 0 SmallHLI
  it "decodes SET 1 B" $ [0b11_001_011, 0b11_001_000] `decodesTo` SET 1 (SmallR8 RegB)
  it "decodes SET 1 C" $ [0b11_001_011, 0b11_001_001] `decodesTo` SET 1 (SmallR8 RegC)
  it "decodes SET 1 D" $ [0b11_001_011, 0b11_001_010] `decodesTo` SET 1 (SmallR8 RegD)
  it "decodes SET 1 E" $ [0b11_001_011, 0b11_001_011] `decodesTo` SET 1 (SmallR8 RegE)
  it "decodes SET 1 H" $ [0b11_001_011, 0b11_001_100] `decodesTo` SET 1 (SmallR8 RegH)
  it "decodes SET 1 L" $ [0b11_001_011, 0b11_001_101] `decodesTo` SET 1 (SmallR8 RegL)
  it "decodes SET 1 A" $ [0b11_001_011, 0b11_001_111] `decodesTo` SET 1 (SmallR8 RegA)
  it "decodes SET 1 (HL)" $ [0b11_001_011, 0b11_001_110] `decodesTo` SET 1 SmallHLI
  it "decodes SET 2 B" $ [0b11_001_011, 0b11_010_000] `decodesTo` SET 2 (SmallR8 RegB)
  it "decodes SET 2 C" $ [0b11_001_011, 0b11_010_001] `decodesTo` SET 2 (SmallR8 RegC)
  it "decodes SET 2 D" $ [0b11_001_011, 0b11_010_010] `decodesTo` SET 2 (SmallR8 RegD)
  it "decodes SET 2 E" $ [0b11_001_011, 0b11_010_011] `decodesTo` SET 2 (SmallR8 RegE)
  it "decodes SET 2 H" $ [0b11_001_011, 0b11_010_100] `decodesTo` SET 2 (SmallR8 RegH)
  it "decodes SET 2 L" $ [0b11_001_011, 0b11_010_101] `decodesTo` SET 2 (SmallR8 RegL)
  it "decodes SET 2 A" $ [0b11_001_011, 0b11_010_111] `decodesTo` SET 2 (SmallR8 RegA)
  it "decodes SET 2 (HL)" $ [0b11_001_011, 0b11_010_110] `decodesTo` SET 2 SmallHLI
  it "decodes SET 3 B" $ [0b11_001_011, 0b11_011_000] `decodesTo` SET 3 (SmallR8 RegB)
  it "decodes SET 3 C" $ [0b11_001_011, 0b11_011_001] `decodesTo` SET 3 (SmallR8 RegC)
  it "decodes SET 3 D" $ [0b11_001_011, 0b11_011_010] `decodesTo` SET 3 (SmallR8 RegD)
  it "decodes SET 3 E" $ [0b11_001_011, 0b11_011_011] `decodesTo` SET 3 (SmallR8 RegE)
  it "decodes SET 3 H" $ [0b11_001_011, 0b11_011_100] `decodesTo` SET 3 (SmallR8 RegH)
  it "decodes SET 3 L" $ [0b11_001_011, 0b11_011_101] `decodesTo` SET 3 (SmallR8 RegL)
  it "decodes SET 3 A" $ [0b11_001_011, 0b11_011_111] `decodesTo` SET 3 (SmallR8 RegA)
  it "decodes SET 3 (HL)" $ [0b11_001_011, 0b11_011_110] `decodesTo` SET 3 SmallHLI
  it "decodes SET 4 B" $ [0b11_001_011, 0b11_100_000] `decodesTo` SET 4 (SmallR8 RegB)
  it "decodes SET 4 C" $ [0b11_001_011, 0b11_100_001] `decodesTo` SET 4 (SmallR8 RegC)
  it "decodes SET 4 D" $ [0b11_001_011, 0b11_100_010] `decodesTo` SET 4 (SmallR8 RegD)
  it "decodes SET 4 E" $ [0b11_001_011, 0b11_100_011] `decodesTo` SET 4 (SmallR8 RegE)
  it "decodes SET 4 H" $ [0b11_001_011, 0b11_100_100] `decodesTo` SET 4 (SmallR8 RegH)
  it "decodes SET 4 L" $ [0b11_001_011, 0b11_100_101] `decodesTo` SET 4 (SmallR8 RegL)
  it "decodes SET 4 A" $ [0b11_001_011, 0b11_100_111] `decodesTo` SET 4 (SmallR8 RegA)
  it "decodes SET 4 (HL)" $ [0b11_001_011, 0b11_100_110] `decodesTo` SET 4 SmallHLI
  it "decodes SET 5 B" $ [0b11_001_011, 0b11_101_000] `decodesTo` SET 5 (SmallR8 RegB)
  it "decodes SET 5 C" $ [0b11_001_011, 0b11_101_001] `decodesTo` SET 5 (SmallR8 RegC)
  it "decodes SET 5 D" $ [0b11_001_011, 0b11_101_010] `decodesTo` SET 5 (SmallR8 RegD)
  it "decodes SET 5 E" $ [0b11_001_011, 0b11_101_011] `decodesTo` SET 5 (SmallR8 RegE)
  it "decodes SET 5 H" $ [0b11_001_011, 0b11_101_100] `decodesTo` SET 5 (SmallR8 RegH)
  it "decodes SET 5 L" $ [0b11_001_011, 0b11_101_101] `decodesTo` SET 5 (SmallR8 RegL)
  it "decodes SET 5 A" $ [0b11_001_011, 0b11_101_111] `decodesTo` SET 5 (SmallR8 RegA)
  it "decodes SET 5 (HL)" $ [0b11_001_011, 0b11_101_110] `decodesTo` SET 5 SmallHLI
  it "decodes SET 6 B" $ [0b11_001_011, 0b11_110_000] `decodesTo` SET 6 (SmallR8 RegB)
  it "decodes SET 6 C" $ [0b11_001_011, 0b11_110_001] `decodesTo` SET 6 (SmallR8 RegC)
  it "decodes SET 6 D" $ [0b11_001_011, 0b11_110_010] `decodesTo` SET 6 (SmallR8 RegD)
  it "decodes SET 6 E" $ [0b11_001_011, 0b11_110_011] `decodesTo` SET 6 (SmallR8 RegE)
  it "decodes SET 6 H" $ [0b11_001_011, 0b11_110_100] `decodesTo` SET 6 (SmallR8 RegH)
  it "decodes SET 6 L" $ [0b11_001_011, 0b11_110_101] `decodesTo` SET 6 (SmallR8 RegL)
  it "decodes SET 6 A" $ [0b11_001_011, 0b11_110_111] `decodesTo` SET 6 (SmallR8 RegA)
  it "decodes SET 6 (HL)" $ [0b11_001_011, 0b11_110_110] `decodesTo` SET 6 SmallHLI
  it "decodes SET 7 B" $ [0b11_001_011, 0b11_111_000] `decodesTo` SET 7 (SmallR8 RegB)
  it "decodes SET 7 C" $ [0b11_001_011, 0b11_111_001] `decodesTo` SET 7 (SmallR8 RegC)
  it "decodes SET 7 D" $ [0b11_001_011, 0b11_111_010] `decodesTo` SET 7 (SmallR8 RegD)
  it "decodes SET 7 E" $ [0b11_001_011, 0b11_111_011] `decodesTo` SET 7 (SmallR8 RegE)
  it "decodes SET 7 H" $ [0b11_001_011, 0b11_111_100] `decodesTo` SET 7 (SmallR8 RegH)
  it "decodes SET 7 L" $ [0b11_001_011, 0b11_111_101] `decodesTo` SET 7 (SmallR8 RegL)
  it "decodes SET 7 A" $ [0b11_001_011, 0b11_111_111] `decodesTo` SET 7 (SmallR8 RegA)
  it "decodes SET 7 (HL)" $ [0b11_001_011, 0b11_111_110] `decodesTo` SET 7 SmallHLI
  it "decodes RES 0 B" $ [0b11_001_011, 0b10_000_000] `decodesTo` RES 0 (SmallR8 RegB)
  it "decodes RES 0 C" $ [0b11_001_011, 0b10_000_001] `decodesTo` RES 0 (SmallR8 RegC)
  it "decodes RES 0 D" $ [0b11_001_011, 0b10_000_010] `decodesTo` RES 0 (SmallR8 RegD)
  it "decodes RES 0 E" $ [0b11_001_011, 0b10_000_011] `decodesTo` RES 0 (SmallR8 RegE)
  it "decodes RES 0 H" $ [0b11_001_011, 0b10_000_100] `decodesTo` RES 0 (SmallR8 RegH)
  it "decodes RES 0 L" $ [0b11_001_011, 0b10_000_101] `decodesTo` RES 0 (SmallR8 RegL)
  it "decodes RES 0 A" $ [0b11_001_011, 0b10_000_111] `decodesTo` RES 0 (SmallR8 RegA)
  it "decodes RES 0 (HL)" $ [0b11_001_011, 0b10_000_110] `decodesTo` RES 0 SmallHLI
  it "decodes RES 1 B" $ [0b11_001_011, 0b10_001_000] `decodesTo` RES 1 (SmallR8 RegB)
  it "decodes RES 1 C" $ [0b11_001_011, 0b10_001_001] `decodesTo` RES 1 (SmallR8 RegC)
  it "decodes RES 1 D" $ [0b11_001_011, 0b10_001_010] `decodesTo` RES 1 (SmallR8 RegD)
  it "decodes RES 1 E" $ [0b11_001_011, 0b10_001_011] `decodesTo` RES 1 (SmallR8 RegE)
  it "decodes RES 1 H" $ [0b11_001_011, 0b10_001_100] `decodesTo` RES 1 (SmallR8 RegH)
  it "decodes RES 1 L" $ [0b11_001_011, 0b10_001_101] `decodesTo` RES 1 (SmallR8 RegL)
  it "decodes RES 1 A" $ [0b11_001_011, 0b10_001_111] `decodesTo` RES 1 (SmallR8 RegA)
  it "decodes RES 1 (HL)" $ [0b11_001_011, 0b10_001_110] `decodesTo` RES 1 SmallHLI
  it "decodes RES 2 B" $ [0b11_001_011, 0b10_010_000] `decodesTo` RES 2 (SmallR8 RegB)
  it "decodes RES 2 C" $ [0b11_001_011, 0b10_010_001] `decodesTo` RES 2 (SmallR8 RegC)
  it "decodes RES 2 D" $ [0b11_001_011, 0b10_010_010] `decodesTo` RES 2 (SmallR8 RegD)
  it "decodes RES 2 E" $ [0b11_001_011, 0b10_010_011] `decodesTo` RES 2 (SmallR8 RegE)
  it "decodes RES 2 H" $ [0b11_001_011, 0b10_010_100] `decodesTo` RES 2 (SmallR8 RegH)
  it "decodes RES 2 L" $ [0b11_001_011, 0b10_010_101] `decodesTo` RES 2 (SmallR8 RegL)
  it "decodes RES 2 A" $ [0b11_001_011, 0b10_010_111] `decodesTo` RES 2 (SmallR8 RegA)
  it "decodes RES 2 (HL)" $ [0b11_001_011, 0b10_010_110] `decodesTo` RES 2 SmallHLI
  it "decodes RES 3 B" $ [0b11_001_011, 0b10_011_000] `decodesTo` RES 3 (SmallR8 RegB)
  it "decodes RES 3 C" $ [0b11_001_011, 0b10_011_001] `decodesTo` RES 3 (SmallR8 RegC)
  it "decodes RES 3 D" $ [0b11_001_011, 0b10_011_010] `decodesTo` RES 3 (SmallR8 RegD)
  it "decodes RES 3 E" $ [0b11_001_011, 0b10_011_011] `decodesTo` RES 3 (SmallR8 RegE)
  it "decodes RES 3 H" $ [0b11_001_011, 0b10_011_100] `decodesTo` RES 3 (SmallR8 RegH)
  it "decodes RES 3 L" $ [0b11_001_011, 0b10_011_101] `decodesTo` RES 3 (SmallR8 RegL)
  it "decodes RES 3 A" $ [0b11_001_011, 0b10_011_111] `decodesTo` RES 3 (SmallR8 RegA)
  it "decodes RES 3 (HL)" $ [0b11_001_011, 0b10_011_110] `decodesTo` RES 3 SmallHLI
  it "decodes RES 4 B" $ [0b11_001_011, 0b10_100_000] `decodesTo` RES 4 (SmallR8 RegB)
  it "decodes RES 4 C" $ [0b11_001_011, 0b10_100_001] `decodesTo` RES 4 (SmallR8 RegC)
  it "decodes RES 4 D" $ [0b11_001_011, 0b10_100_010] `decodesTo` RES 4 (SmallR8 RegD)
  it "decodes RES 4 E" $ [0b11_001_011, 0b10_100_011] `decodesTo` RES 4 (SmallR8 RegE)
  it "decodes RES 4 H" $ [0b11_001_011, 0b10_100_100] `decodesTo` RES 4 (SmallR8 RegH)
  it "decodes RES 4 L" $ [0b11_001_011, 0b10_100_101] `decodesTo` RES 4 (SmallR8 RegL)
  it "decodes RES 4 A" $ [0b11_001_011, 0b10_100_111] `decodesTo` RES 4 (SmallR8 RegA)
  it "decodes RES 4 (HL)" $ [0b11_001_011, 0b10_100_110] `decodesTo` RES 4 SmallHLI
  it "decodes RES 5 B" $ [0b11_001_011, 0b10_101_000] `decodesTo` RES 5 (SmallR8 RegB)
  it "decodes RES 5 C" $ [0b11_001_011, 0b10_101_001] `decodesTo` RES 5 (SmallR8 RegC)
  it "decodes RES 5 D" $ [0b11_001_011, 0b10_101_010] `decodesTo` RES 5 (SmallR8 RegD)
  it "decodes RES 5 E" $ [0b11_001_011, 0b10_101_011] `decodesTo` RES 5 (SmallR8 RegE)
  it "decodes RES 5 H" $ [0b11_001_011, 0b10_101_100] `decodesTo` RES 5 (SmallR8 RegH)
  it "decodes RES 5 L" $ [0b11_001_011, 0b10_101_101] `decodesTo` RES 5 (SmallR8 RegL)
  it "decodes RES 5 A" $ [0b11_001_011, 0b10_101_111] `decodesTo` RES 5 (SmallR8 RegA)
  it "decodes RES 5 (HL)" $ [0b11_001_011, 0b10_101_110] `decodesTo` RES 5 SmallHLI
  it "decodes RES 6 B" $ [0b11_001_011, 0b10_110_000] `decodesTo` RES 6 (SmallR8 RegB)
  it "decodes RES 6 C" $ [0b11_001_011, 0b10_110_001] `decodesTo` RES 6 (SmallR8 RegC)
  it "decodes RES 6 D" $ [0b11_001_011, 0b10_110_010] `decodesTo` RES 6 (SmallR8 RegD)
  it "decodes RES 6 E" $ [0b11_001_011, 0b10_110_011] `decodesTo` RES 6 (SmallR8 RegE)
  it "decodes RES 6 H" $ [0b11_001_011, 0b10_110_100] `decodesTo` RES 6 (SmallR8 RegH)
  it "decodes RES 6 L" $ [0b11_001_011, 0b10_110_101] `decodesTo` RES 6 (SmallR8 RegL)
  it "decodes RES 6 A" $ [0b11_001_011, 0b10_110_111] `decodesTo` RES 6 (SmallR8 RegA)
  it "decodes RES 6 (HL)" $ [0b11_001_011, 0b10_110_110] `decodesTo` RES 6 SmallHLI
  it "decodes RES 7 B" $ [0b11_001_011, 0b10_111_000] `decodesTo` RES 7 (SmallR8 RegB)
  it "decodes RES 7 C" $ [0b11_001_011, 0b10_111_001] `decodesTo` RES 7 (SmallR8 RegC)
  it "decodes RES 7 D" $ [0b11_001_011, 0b10_111_010] `decodesTo` RES 7 (SmallR8 RegD)
  it "decodes RES 7 E" $ [0b11_001_011, 0b10_111_011] `decodesTo` RES 7 (SmallR8 RegE)
  it "decodes RES 7 H" $ [0b11_001_011, 0b10_111_100] `decodesTo` RES 7 (SmallR8 RegH)
  it "decodes RES 7 L" $ [0b11_001_011, 0b10_111_101] `decodesTo` RES 7 (SmallR8 RegL)
  it "decodes RES 7 A" $ [0b11_001_011, 0b10_111_111] `decodesTo` RES 7 (SmallR8 RegA)
  it "decodes RES 7 (HL)" $ [0b11_001_011, 0b10_111_110] `decodesTo` RES 7 SmallHLI
  it "decodes JP n" $ [0b11_000_011, 0x32, 0x42] `decodesTo` JP 0x4232
  it "decodes JP NZ n" $ [0b11_000_010, 0x32, 0x42] `decodesTo` JPCC CondNZ 0x4232
  it "decodes JP Z n" $ [0b11_001_010, 0x32, 0x42] `decodesTo` JPCC CondZ 0x4232
  it "decodes JP NC n" $ [0b11_010_010, 0x32, 0x42] `decodesTo` JPCC CondNC 0x4232
  it "decodes JP C n" $ [0b11_011_010, 0x32, 0x42] `decodesTo` JPCC CondC 0x4232
  it "decodes JP (HL)" $ [0b11_101_001] `decodesTo` JPI
  it "decodes JR n" $ [0b00_011_000, 0x42] `decodesTo` JR 0x42
  it "decodes JR NZ n" $ [0b00_100_000, 0x42] `decodesTo` JRCC CondNZ 0x42
  it "decodes JR Z n" $ [0b00_101_000, 0x42] `decodesTo` JRCC CondZ 0x42
  it "decodes JR NC n" $ [0b00_110_000, 0x42] `decodesTo` JRCC CondNC 0x42
  it "decodes JR C n" $ [0b00_111_000, 0x42] `decodesTo` JRCC CondC 0x42
  it "decodes CALL n" $ [0b11_001_101, 0x32, 0x42] `decodesTo` CALL 0x4232
  it "decodes CALL NZ n" $ [0b11_000_100, 0x32, 0x42] `decodesTo` CALLCC CondNZ 0x4232
  it "decodes CALL Z n" $ [0b11_001_100, 0x32, 0x42] `decodesTo` CALLCC CondZ 0x4232
  it "decodes CALL NC n" $ [0b11_010_100, 0x32, 0x42] `decodesTo` CALLCC CondNC 0x4232
  it "decodes CALL C n" $ [0b11_011_100, 0x32, 0x42] `decodesTo` CALLCC CondC 0x4232
  it "decodes RET" $ [0b11_001_001] `decodesTo` RET
  it "decodes RET NZ" $ [0b11_000_000] `decodesTo` RETCC CondNZ
  it "decodes RET Z" $ [0b11_001_000] `decodesTo` RETCC CondZ
  it "decodes RET NC" $ [0b11_010_000] `decodesTo` RETCC CondNC
  it "decodes RET C" $ [0b11_011_000] `decodesTo` RETCC CondC
  it "decodes RETI" $ [0b11_011_001] `decodesTo` RETI
  it "decodes RST n" $ [0b11_000_111] `decodesTo` RST 0
  it "decodes RST n" $ [0b11_001_111] `decodesTo` RST 1
  it "decodes RST n" $ [0b11_010_111] `decodesTo` RST 2
  it "decodes RST n" $ [0b11_011_111] `decodesTo` RST 3
  it "decodes RST n" $ [0b11_100_111] `decodesTo` RST 4
  it "decodes RST n" $ [0b11_101_111] `decodesTo` RST 5
  it "decodes RST n" $ [0b11_110_111] `decodesTo` RST 6
  it "decodes RST n" $ [0b11_111_111] `decodesTo` RST 7
  it "decodes DAA" $ [0b00_100_111] `decodesTo` DAA
  it "decodes CPL" $ [0b00_101_111] `decodesTo` CPL
  it "decodes NOP" $ [0b00_000_000] `decodesTo` NOP
  it "decodes CCF" $ [0b00_111_111] `decodesTo` CCF
  it "decodes SCF" $ [0b00_110_111] `decodesTo` SCF
  it "decodes DI" $ [0b11_110_011] `decodesTo` DI
  it "decodes EI" $ [0b11_111_011] `decodesTo` EI
  it "decodes HALT" $ [0b01_110_110] `decodesTo` HALT
  it "decodes STOP" $ [0b00_010_000, 0] `decodesTo` STOP
  it "does not decode invalid STOP" $ [0b00_010_000, 1] `decodesTo` INVALID 0b00_010_000
  it "does not decode invalid instruction 11_100 100"
    $           [0b11_100_100]
    `decodesTo` INVALID 0b11_100_100
  it "does not decode invalid instruction 11_101 100"
    $           [0b11_101_100]
    `decodesTo` INVALID 0b11_101_100
  it "does not decode invalid instruction 11_110 100"
    $           [0b11_110_100]
    `decodesTo` INVALID 0b11_110_100
  it "does not decode invalid instruction 11_111 100"
    $           [0b11_111_100]
    `decodesTo` INVALID 0b11_111_100
  it "does not decode invalid instruction 11_011 101"
    $           [0b11_011_101]
    `decodesTo` INVALID 0b11_011_101
  it "does not decode invalid instruction 11_101 101"
    $           [0b11_101_101]
    `decodesTo` INVALID 0b11_101_101
  it "does not decode invalid instruction 11_111 101"
    $           [0b11_111_101]
    `decodesTo` INVALID 0b11_111_101
