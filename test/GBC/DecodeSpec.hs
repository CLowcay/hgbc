{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

module GBC.DecodeSpec where

import           Data.Word
import           GBC.CPU.Decode
import           GBC.CPU.ISA
import           Data.Bits
import           Common
import           Test.Hspec
import           Control.Monad.State
import           Control.Monad.Reader

instance MonadFetch (DisassembleT (StateT [Word8] Maybe)) where
  nextByte = lift $ do
    b0 : bs <- get
    put bs
    pure b0
  nextWord = lift $ do
    b0 : b1 : bs <- get
    put bs
    pure ((fromIntegral b1 .<<. 8) .|. fromIntegral b0)

decodesTo :: [Word8] -> String -> IO ()
decodesTo encoding expectedDecoding = do
  let result = runStateT (runDisassembleT fetchAndExecute (const Nothing)) encoding
  case result of
    Nothing                    -> expectationFailure "Failed to decode instruction"
    Just (decoding, remainder) -> do
      decoding `shouldBe` expectedDecoding
      remainder `shouldBe` []

spec :: Spec
spec = describe "decode" $ do
  it "decodes LD B, B" $ [0b01_000_000] `decodesTo` "LD B, B"
  it "decodes LD C, B" $ [0b01_001_000] `decodesTo` "LD C, B"
  it "decodes LD D, B" $ [0b01_010_000] `decodesTo` "LD D, B"
  it "decodes LD E, B" $ [0b01_011_000] `decodesTo` "LD E, B"
  it "decodes LD H, B" $ [0b01_100_000] `decodesTo` "LD H, B"
  it "decodes LD L, B" $ [0b01_101_000] `decodesTo` "LD L, B"
  it "decodes LD A, B" $ [0b01_111_000] `decodesTo` "LD A, B"
  it "decodes LD B, C" $ [0b01_000_001] `decodesTo` "LD B, C"
  it "decodes LD C, C" $ [0b01_001_001] `decodesTo` "LD C, C"
  it "decodes LD D, C" $ [0b01_010_001] `decodesTo` "LD D, C"
  it "decodes LD E, C" $ [0b01_011_001] `decodesTo` "LD E, C"
  it "decodes LD H, C" $ [0b01_100_001] `decodesTo` "LD H, C"
  it "decodes LD L, C" $ [0b01_101_001] `decodesTo` "LD L, C"
  it "decodes LD A, C" $ [0b01_111_001] `decodesTo` "LD A, C"
  it "decodes LD B, D" $ [0b01_000_010] `decodesTo` "LD B, D"
  it "decodes LD C, D" $ [0b01_001_010] `decodesTo` "LD C, D"
  it "decodes LD D, D" $ [0b01_010_010] `decodesTo` "LD D, D"
  it "decodes LD E, D" $ [0b01_011_010] `decodesTo` "LD E, D"
  it "decodes LD H, D" $ [0b01_100_010] `decodesTo` "LD H, D"
  it "decodes LD L, D" $ [0b01_101_010] `decodesTo` "LD L, D"
  it "decodes LD A, D" $ [0b01_111_010] `decodesTo` "LD A, D"
  it "decodes LD B, E" $ [0b01_000_011] `decodesTo` "LD B, E"
  it "decodes LD C, E" $ [0b01_001_011] `decodesTo` "LD C, E"
  it "decodes LD D, E" $ [0b01_010_011] `decodesTo` "LD D, E"
  it "decodes LD E, E" $ [0b01_011_011] `decodesTo` "LD E, E"
  it "decodes LD H, E" $ [0b01_100_011] `decodesTo` "LD H, E"
  it "decodes LD L, E" $ [0b01_101_011] `decodesTo` "LD L, E"
  it "decodes LD A, E" $ [0b01_111_011] `decodesTo` "LD A, E"
  it "decodes LD B, H" $ [0b01_000_100] `decodesTo` "LD B, H"
  it "decodes LD C, H" $ [0b01_001_100] `decodesTo` "LD C, H"
  it "decodes LD D, H" $ [0b01_010_100] `decodesTo` "LD D, H"
  it "decodes LD E, H" $ [0b01_011_100] `decodesTo` "LD E, H"
  it "decodes LD H, H" $ [0b01_100_100] `decodesTo` "LD H, H"
  it "decodes LD L, H" $ [0b01_101_100] `decodesTo` "LD L, H"
  it "decodes LD A, H" $ [0b01_111_100] `decodesTo` "LD A, H"
  it "decodes LD B, L" $ [0b01_000_101] `decodesTo` "LD B, L"
  it "decodes LD C, L" $ [0b01_001_101] `decodesTo` "LD C, L"
  it "decodes LD D, L" $ [0b01_010_101] `decodesTo` "LD D, L"
  it "decodes LD E, L" $ [0b01_011_101] `decodesTo` "LD E, L"
  it "decodes LD H, L" $ [0b01_100_101] `decodesTo` "LD H, L"
  it "decodes LD L, L" $ [0b01_101_101] `decodesTo` "LD L, L"
  it "decodes LD A, L" $ [0b01_111_101] `decodesTo` "LD A, L"
  it "decodes LD B, n" $ [0b00_000_110, 0x42] `decodesTo` "LD B, 42"
  it "decodes LD C, n" $ [0b00_001_110, 0x42] `decodesTo` "LD C, 42"
  it "decodes LD D, n" $ [0b00_010_110, 0x42] `decodesTo` "LD D, 42"
  it "decodes LD E, n" $ [0b00_011_110, 0x42] `decodesTo` "LD E, 42"
  it "decodes LD H, n" $ [0b00_100_110, 0x42] `decodesTo` "LD H, 42"
  it "decodes LD L, n" $ [0b00_101_110, 0x42] `decodesTo` "LD L, 42"
  it "decodes LD A, n" $ [0b00_111_110, 0x42] `decodesTo` "LD A, 42"
  it "decodes LD B, (HL)" $ [0b01_000_110] `decodesTo` "LD B, (HL)"
  it "decodes LD C, (HL)" $ [0b01_001_110] `decodesTo` "LD C, (HL)"
  it "decodes LD D, (HL)" $ [0b01_010_110] `decodesTo` "LD D, (HL)"
  it "decodes LD E, (HL)" $ [0b01_011_110] `decodesTo` "LD E, (HL)"
  it "decodes LD H, (HL)" $ [0b01_100_110] `decodesTo` "LD H, (HL)"
  it "decodes LD L, (HL)" $ [0b01_101_110] `decodesTo` "LD L, (HL)"
  it "decodes LD A, (HL)" $ [0b01_111_110] `decodesTo` "LD A, (HL)"
  it "decodes LD (HL), B" $ [0b01_110_000] `decodesTo` "LD (HL), B"
  it "decodes LD (HL), C" $ [0b01_110_001] `decodesTo` "LD (HL), C"
  it "decodes LD (HL), D" $ [0b01_110_010] `decodesTo` "LD (HL), D"
  it "decodes LD (HL), E" $ [0b01_110_011] `decodesTo` "LD (HL), E"
  it "decodes LD (HL), H" $ [0b01_110_100] `decodesTo` "LD (HL), H"
  it "decodes LD (HL), L" $ [0b01_110_101] `decodesTo` "LD (HL), L"
  it "decodes LD (HL), A" $ [0b01_110_111] `decodesTo` "LD (HL), A"
  it "decodes LD (HL), n" $ [0b00_110_110, 0x42] `decodesTo` "LD (HL), 42"
  it "decodes LD A, (BC)" $ [0b00_001_010] `decodesTo` "LD A, (BC)"
  it "decodes LD A, (DE)" $ [0b00_011_010] `decodesTo` "LD A, (DE)"
  it "decodes LD A, (C)" $ [0b11_110_010] `decodesTo` "LD A, (C)"
  it "decodes LD (C), A" $ [0b11_100_010] `decodesTo` "LD (C), A"
  it "decodes LD A, (n)" $ [0b11_110_000, 0x42] `decodesTo` "LD A, (FF42)"
  it "decodes LD (n), A" $ [0b11_100_000, 0x42] `decodesTo` "LD (FF42), A"
  it "decodes LD A, (nn)" $ [0b11_111_010, 0x42, 0x32] `decodesTo` "LD A, (3242)"
  it "decodes LD (nn), A" $ [0b11_101_010, 0x42, 0x32] `decodesTo` "LD (3242), A"
  it "decodes LD A, (HLI)" $ [0b00_101_010] `decodesTo` "LD A, (HLI)"
  it "decodes LD A, (HLD)" $ [0b00_111_010] `decodesTo` "LD A, (HLD)"
  it "decodes LD (BC), A" $ [0b00_000_010] `decodesTo` "LD (BC), A"
  it "decodes LD (DE), A" $ [0b00_010_010] `decodesTo` "LD (DE), A"
  it "decodes LD (HLI), A" $ [0b00_100_010] `decodesTo` "LD (HLI), A"
  it "decodes LD (HLD), A" $ [0b00_110_010] `decodesTo` "LD (HLD), A"
  it "decodes LD BC, n" $ [0b00_000_001, 0x42, 0x32] `decodesTo` "LD BC, 3242"
  it "decodes LD DE, n" $ [0b00_010_001, 0x42, 0x32] `decodesTo` "LD DE, 3242"
  it "decodes LD HL, n" $ [0b00_100_001, 0x42, 0x32] `decodesTo` "LD HL, 3242"
  it "decodes LD SP, n" $ [0b00_110_001, 0x42, 0x32] `decodesTo` "LD SP, 3242"
  it "decodes LD SP, HL" $ [0b11_111_001] `decodesTo` "LD SP, HL"
  it "decodes PUSH BC" $ [0b11_000_101] `decodesTo` "PUSH BC"
  it "decodes PUSH DE" $ [0b11_010_101] `decodesTo` "PUSH DE"
  it "decodes PUSH HL" $ [0b11_100_101] `decodesTo` "PUSH HL"
  it "decodes PUSH AF" $ [0b11_110_101] `decodesTo` "PUSH AF"
  it "decodes POP BC" $ [0b11_000_001] `decodesTo` "POP BC"
  it "decodes POP DE" $ [0b11_010_001] `decodesTo` "POP DE"
  it "decodes POP HL" $ [0b11_100_001] `decodesTo` "POP HL"
  it "decodes POP AF" $ [0b11_110_001] `decodesTo` "POP AF"
  it "decodes LDHL SP, e" $ [0b11_111_000, 0x42] `decodesTo` "LDHL SP, 42"
  it "decodes LD (n), SP" $ [0b00_001_000, 0x42, 0x32] `decodesTo` "LD (3242), SP"
  it "decodes ADD A, B" $ [0b10_000_000] `decodesTo` "ADD A, B"
  it "decodes ADD A, C" $ [0b10_000_001] `decodesTo` "ADD A, C"
  it "decodes ADD A, D" $ [0b10_000_010] `decodesTo` "ADD A, D"
  it "decodes ADD A, E" $ [0b10_000_011] `decodesTo` "ADD A, E"
  it "decodes ADD A, H" $ [0b10_000_100] `decodesTo` "ADD A, H"
  it "decodes ADD A, L" $ [0b10_000_101] `decodesTo` "ADD A, L"
  it "decodes ADD A, A" $ [0b10_000_111] `decodesTo` "ADD A, A"
  it "decodes ADD A, n" $ [0b11_000_110, 0x42] `decodesTo` "ADD A, 42"
  it "decodes ADD A, (HL)" $ [0b10_000_110] `decodesTo` "ADD A, (HL)"
  it "decodes ADC A, B" $ [0b10_001_000] `decodesTo` "ADC A, B"
  it "decodes ADC A, C" $ [0b10_001_001] `decodesTo` "ADC A, C"
  it "decodes ADC A, D" $ [0b10_001_010] `decodesTo` "ADC A, D"
  it "decodes ADC A, E" $ [0b10_001_011] `decodesTo` "ADC A, E"
  it "decodes ADC A, H" $ [0b10_001_100] `decodesTo` "ADC A, H"
  it "decodes ADC A, L" $ [0b10_001_101] `decodesTo` "ADC A, L"
  it "decodes ADC A, A" $ [0b10_001_111] `decodesTo` "ADC A, A"
  it "decodes ADC A, n" $ [0b11_001_110, 0x42] `decodesTo` "ADC A, 42"
  it "decodes ADC A, (HL)" $ [0b10_001_110] `decodesTo` "ADC A, (HL)"
  it "decodes SUB A, B" $ [0b10_010_000] `decodesTo` "SUB A, B"
  it "decodes SUB A, C" $ [0b10_010_001] `decodesTo` "SUB A, C"
  it "decodes SUB A, D" $ [0b10_010_010] `decodesTo` "SUB A, D"
  it "decodes SUB A, E" $ [0b10_010_011] `decodesTo` "SUB A, E"
  it "decodes SUB A, H" $ [0b10_010_100] `decodesTo` "SUB A, H"
  it "decodes SUB A, L" $ [0b10_010_101] `decodesTo` "SUB A, L"
  it "decodes SUB A, A" $ [0b10_010_111] `decodesTo` "SUB A, A"
  it "decodes SUB A, n" $ [0b11_010_110, 0x42] `decodesTo` "SUB A, 42"
  it "decodes SUB A, (HL)" $ [0b10_010_110] `decodesTo` "SUB A, (HL)"
  it "decodes SBC A, B" $ [0b10_011_000] `decodesTo` "SBC A, B"
  it "decodes SBC A, C" $ [0b10_011_001] `decodesTo` "SBC A, C"
  it "decodes SBC A, D" $ [0b10_011_010] `decodesTo` "SBC A, D"
  it "decodes SBC A, E" $ [0b10_011_011] `decodesTo` "SBC A, E"
  it "decodes SBC A, H" $ [0b10_011_100] `decodesTo` "SBC A, H"
  it "decodes SBC A, L" $ [0b10_011_101] `decodesTo` "SBC A, L"
  it "decodes SBC A, A" $ [0b10_011_111] `decodesTo` "SBC A, A"
  it "decodes SBC A, n" $ [0b11_011_110, 0x42] `decodesTo` "SBC A, 42"
  it "decodes SBC A, (HL)" $ [0b10_011_110] `decodesTo` "SBC A, (HL)"
  it "decodes AND A, B" $ [0b10_100_000] `decodesTo` "AND A, B"
  it "decodes AND A, C" $ [0b10_100_001] `decodesTo` "AND A, C"
  it "decodes AND A, D" $ [0b10_100_010] `decodesTo` "AND A, D"
  it "decodes AND A, E" $ [0b10_100_011] `decodesTo` "AND A, E"
  it "decodes AND A, H" $ [0b10_100_100] `decodesTo` "AND A, H"
  it "decodes AND A, L" $ [0b10_100_101] `decodesTo` "AND A, L"
  it "decodes AND A, A" $ [0b10_100_111] `decodesTo` "AND A, A"
  it "decodes AND A, n" $ [0b11_100_110, 0x42] `decodesTo` "AND A, 42"
  it "decodes AND A, (HL)" $ [0b10_100_110] `decodesTo` "AND A, (HL)"
  it "decodes XOR A, B" $ [0b10_101_000] `decodesTo` "XOR A, B"
  it "decodes XOR A, C" $ [0b10_101_001] `decodesTo` "XOR A, C"
  it "decodes XOR A, D" $ [0b10_101_010] `decodesTo` "XOR A, D"
  it "decodes XOR A, E" $ [0b10_101_011] `decodesTo` "XOR A, E"
  it "decodes XOR A, H" $ [0b10_101_100] `decodesTo` "XOR A, H"
  it "decodes XOR A, L" $ [0b10_101_101] `decodesTo` "XOR A, L"
  it "decodes XOR A, A" $ [0b10_101_111] `decodesTo` "XOR A, A"
  it "decodes XOR A, n" $ [0b11_101_110, 0x42] `decodesTo` "XOR A, 42"
  it "decodes XOR A, (HL)" $ [0b10_101_110] `decodesTo` "XOR A, (HL)"
  it "decodes OR A, B" $ [0b10_110_000] `decodesTo` "OR A, B"
  it "decodes OR A, C" $ [0b10_110_001] `decodesTo` "OR A, C"
  it "decodes OR A, D" $ [0b10_110_010] `decodesTo` "OR A, D"
  it "decodes OR A, E" $ [0b10_110_011] `decodesTo` "OR A, E"
  it "decodes OR A, H" $ [0b10_110_100] `decodesTo` "OR A, H"
  it "decodes OR A, L" $ [0b10_110_101] `decodesTo` "OR A, L"
  it "decodes OR A, A" $ [0b10_110_111] `decodesTo` "OR A, A"
  it "decodes OR A, n" $ [0b11_110_110, 0x42] `decodesTo` "OR A, 42"
  it "decodes OR A, (HL)" $ [0b10_110_110] `decodesTo` "OR A, (HL)"
  it "decodes CP A, B" $ [0b10_111_000] `decodesTo` "CP A, B"
  it "decodes CP A, C" $ [0b10_111_001] `decodesTo` "CP A, C"
  it "decodes CP A, D" $ [0b10_111_010] `decodesTo` "CP A, D"
  it "decodes CP A, E" $ [0b10_111_011] `decodesTo` "CP A, E"
  it "decodes CP A, H" $ [0b10_111_100] `decodesTo` "CP A, H"
  it "decodes CP A, L" $ [0b10_111_101] `decodesTo` "CP A, L"
  it "decodes CP A, A" $ [0b10_111_111] `decodesTo` "CP A, A"
  it "decodes CP A, n" $ [0b11_111_110, 0x42] `decodesTo` "CP A, 42"
  it "decodes CP A, (HL)" $ [0b10_111_110] `decodesTo` "CP A, (HL)"
  it "decodes INC B" $ [0b00_000_100] `decodesTo` "INC B"
  it "decodes INC C" $ [0b00_001_100] `decodesTo` "INC C"
  it "decodes INC D" $ [0b00_010_100] `decodesTo` "INC D"
  it "decodes INC E" $ [0b00_011_100] `decodesTo` "INC E"
  it "decodes INC H" $ [0b00_100_100] `decodesTo` "INC H"
  it "decodes INC L" $ [0b00_101_100] `decodesTo` "INC L"
  it "decodes INC A" $ [0b00_111_100] `decodesTo` "INC A"
  it "decodes INC (HL)" $ [0b00_110_100] `decodesTo` "INC (HL)"
  it "decodes DEC B" $ [0b00_000_101] `decodesTo` "DEC B"
  it "decodes DEC C" $ [0b00_001_101] `decodesTo` "DEC C"
  it "decodes DEC D" $ [0b00_010_101] `decodesTo` "DEC D"
  it "decodes DEC E" $ [0b00_011_101] `decodesTo` "DEC E"
  it "decodes DEC H" $ [0b00_100_101] `decodesTo` "DEC H"
  it "decodes DEC L" $ [0b00_101_101] `decodesTo` "DEC L"
  it "decodes DEC A" $ [0b00_111_101] `decodesTo` "DEC A"
  it "decodes DEC (HL)" $ [0b00_110_101] `decodesTo` "DEC (HL)"
  it "decodes ADD HL BC" $ [0b00_001_001] `decodesTo` "ADD HL, BC"
  it "decodes ADD HL DE" $ [0b00_011_001] `decodesTo` "ADD HL, DE"
  it "decodes ADD HL HL" $ [0b00_101_001] `decodesTo` "ADD HL, HL"
  it "decodes ADD HL SP" $ [0b00_111_001] `decodesTo` "ADD HL, SP"
  it "decodes ADD SP e" $ [0b11_101_000, 0x42] `decodesTo` "ADD SP, 42"
  it "decodes INC BC" $ [0b00_000_011] `decodesTo` "INC BC"
  it "decodes INC DE" $ [0b00_010_011] `decodesTo` "INC DE"
  it "decodes INC HL" $ [0b00_100_011] `decodesTo` "INC HL"
  it "decodes INC SP" $ [0b00_110_011] `decodesTo` "INC SP"
  it "decodes DEC BC" $ [0b00_001_011] `decodesTo` "DEC BC"
  it "decodes DEC DE" $ [0b00_011_011] `decodesTo` "DEC DE"
  it "decodes DEC HL" $ [0b00_101_011] `decodesTo` "DEC HL"
  it "decodes DEC SP" $ [0b00_111_011] `decodesTo` "DEC SP"
  it "decodes RLCA" $ [0b00_000_111] `decodesTo` "RLCA"
  it "decodes RLA" $ [0b00_010_111] `decodesTo` "RLA"
  it "decodes RRCA" $ [0b00_001_111] `decodesTo` "RRCA"
  it "decodes RRA" $ [0b00_011_111] `decodesTo` "RRA"
  it "decodes RLC B" $ [0b11_001_011, 0b00_000_000] `decodesTo` "RLC B"
  it "decodes RLC C" $ [0b11_001_011, 0b00_000_001] `decodesTo` "RLC C"
  it "decodes RLC D" $ [0b11_001_011, 0b00_000_010] `decodesTo` "RLC D"
  it "decodes RLC E" $ [0b11_001_011, 0b00_000_011] `decodesTo` "RLC E"
  it "decodes RLC H" $ [0b11_001_011, 0b00_000_100] `decodesTo` "RLC H"
  it "decodes RLC L" $ [0b11_001_011, 0b00_000_101] `decodesTo` "RLC L"
  it "decodes RLC A" $ [0b11_001_011, 0b00_000_111] `decodesTo` "RLC A"
  it "decodes RLC (HL)" $ [0b11_001_011, 0b00_000_110] `decodesTo` "RLC (HL)"
  it "decodes RL B" $ [0b11_001_011, 0b00_010_000] `decodesTo` "RL B"
  it "decodes RL C" $ [0b11_001_011, 0b00_010_001] `decodesTo` "RL C"
  it "decodes RL D" $ [0b11_001_011, 0b00_010_010] `decodesTo` "RL D"
  it "decodes RL E" $ [0b11_001_011, 0b00_010_011] `decodesTo` "RL E"
  it "decodes RL H" $ [0b11_001_011, 0b00_010_100] `decodesTo` "RL H"
  it "decodes RL L" $ [0b11_001_011, 0b00_010_101] `decodesTo` "RL L"
  it "decodes RL A" $ [0b11_001_011, 0b00_010_111] `decodesTo` "RL A"
  it "decodes RL (HL)" $ [0b11_001_011, 0b00_010_110] `decodesTo` "RL (HL)"
  it "decodes RRC B" $ [0b11_001_011, 0b00_001_000] `decodesTo` "RRC B"
  it "decodes RRC C" $ [0b11_001_011, 0b00_001_001] `decodesTo` "RRC C"
  it "decodes RRC D" $ [0b11_001_011, 0b00_001_010] `decodesTo` "RRC D"
  it "decodes RRC E" $ [0b11_001_011, 0b00_001_011] `decodesTo` "RRC E"
  it "decodes RRC H" $ [0b11_001_011, 0b00_001_100] `decodesTo` "RRC H"
  it "decodes RRC L" $ [0b11_001_011, 0b00_001_101] `decodesTo` "RRC L"
  it "decodes RRC A" $ [0b11_001_011, 0b00_001_111] `decodesTo` "RRC A"
  it "decodes RRC (HL)" $ [0b11_001_011, 0b00_001_110] `decodesTo` "RRC (HL)"
  it "decodes RR B" $ [0b11_001_011, 0b00_011_000] `decodesTo` "RR B"
  it "decodes RR C" $ [0b11_001_011, 0b00_011_001] `decodesTo` "RR C"
  it "decodes RR D" $ [0b11_001_011, 0b00_011_010] `decodesTo` "RR D"
  it "decodes RR E" $ [0b11_001_011, 0b00_011_011] `decodesTo` "RR E"
  it "decodes RR H" $ [0b11_001_011, 0b00_011_100] `decodesTo` "RR H"
  it "decodes RR L" $ [0b11_001_011, 0b00_011_101] `decodesTo` "RR L"
  it "decodes RR A" $ [0b11_001_011, 0b00_011_111] `decodesTo` "RR A"
  it "decodes RR (HL)" $ [0b11_001_011, 0b00_011_110] `decodesTo` "RR (HL)"
  it "decodes SLA B" $ [0b11_001_011, 0b00_100_000] `decodesTo` "SLA B"
  it "decodes SLA C" $ [0b11_001_011, 0b00_100_001] `decodesTo` "SLA C"
  it "decodes SLA D" $ [0b11_001_011, 0b00_100_010] `decodesTo` "SLA D"
  it "decodes SLA E" $ [0b11_001_011, 0b00_100_011] `decodesTo` "SLA E"
  it "decodes SLA H" $ [0b11_001_011, 0b00_100_100] `decodesTo` "SLA H"
  it "decodes SLA L" $ [0b11_001_011, 0b00_100_101] `decodesTo` "SLA L"
  it "decodes SLA A" $ [0b11_001_011, 0b00_100_111] `decodesTo` "SLA A"
  it "decodes SLA (HL)" $ [0b11_001_011, 0b00_100_110] `decodesTo` "SLA (HL)"
  it "decodes SRA B" $ [0b11_001_011, 0b00_101_000] `decodesTo` "SRA B"
  it "decodes SRA C" $ [0b11_001_011, 0b00_101_001] `decodesTo` "SRA C"
  it "decodes SRA D" $ [0b11_001_011, 0b00_101_010] `decodesTo` "SRA D"
  it "decodes SRA E" $ [0b11_001_011, 0b00_101_011] `decodesTo` "SRA E"
  it "decodes SRA H" $ [0b11_001_011, 0b00_101_100] `decodesTo` "SRA H"
  it "decodes SRA L" $ [0b11_001_011, 0b00_101_101] `decodesTo` "SRA L"
  it "decodes SRA A" $ [0b11_001_011, 0b00_101_111] `decodesTo` "SRA A"
  it "decodes SRA (HL)" $ [0b11_001_011, 0b00_101_110] `decodesTo` "SRA (HL)"
  it "decodes SRL B" $ [0b11_001_011, 0b00_111_000] `decodesTo` "SRL B"
  it "decodes SRL C" $ [0b11_001_011, 0b00_111_001] `decodesTo` "SRL C"
  it "decodes SRL D" $ [0b11_001_011, 0b00_111_010] `decodesTo` "SRL D"
  it "decodes SRL E" $ [0b11_001_011, 0b00_111_011] `decodesTo` "SRL E"
  it "decodes SRL H" $ [0b11_001_011, 0b00_111_100] `decodesTo` "SRL H"
  it "decodes SRL L" $ [0b11_001_011, 0b00_111_101] `decodesTo` "SRL L"
  it "decodes SRL A" $ [0b11_001_011, 0b00_111_111] `decodesTo` "SRL A"
  it "decodes SRL (HL)" $ [0b11_001_011, 0b00_111_110] `decodesTo` "SRL (HL)"
  it "decodes SWAP B" $ [0b11_001_011, 0b00_110_000] `decodesTo` "SWAP B"
  it "decodes SWAP C" $ [0b11_001_011, 0b00_110_001] `decodesTo` "SWAP C"
  it "decodes SWAP D" $ [0b11_001_011, 0b00_110_010] `decodesTo` "SWAP D"
  it "decodes SWAP E" $ [0b11_001_011, 0b00_110_011] `decodesTo` "SWAP E"
  it "decodes SWAP H" $ [0b11_001_011, 0b00_110_100] `decodesTo` "SWAP H"
  it "decodes SWAP L" $ [0b11_001_011, 0b00_110_101] `decodesTo` "SWAP L"
  it "decodes SWAP A" $ [0b11_001_011, 0b00_110_111] `decodesTo` "SWAP A"
  it "decodes SWAP (HL)" $ [0b11_001_011, 0b00_110_110] `decodesTo` "SWAP (HL)"
  it "decodes BIT 0, B" $ [0b11_001_011, 0b01_000_000] `decodesTo` "BIT 0, B"
  it "decodes BIT 0, C" $ [0b11_001_011, 0b01_000_001] `decodesTo` "BIT 0, C"
  it "decodes BIT 0, D" $ [0b11_001_011, 0b01_000_010] `decodesTo` "BIT 0, D"
  it "decodes BIT 0, E" $ [0b11_001_011, 0b01_000_011] `decodesTo` "BIT 0, E"
  it "decodes BIT 0, H" $ [0b11_001_011, 0b01_000_100] `decodesTo` "BIT 0, H"
  it "decodes BIT 0, L" $ [0b11_001_011, 0b01_000_101] `decodesTo` "BIT 0, L"
  it "decodes BIT 0, A" $ [0b11_001_011, 0b01_000_111] `decodesTo` "BIT 0, A"
  it "decodes BIT 0, (HL)" $ [0b11_001_011, 0b01_000_110] `decodesTo` "BIT 0, (HL)"
  it "decodes BIT 1, B" $ [0b11_001_011, 0b01_001_000] `decodesTo` "BIT 1, B"
  it "decodes BIT 1, C" $ [0b11_001_011, 0b01_001_001] `decodesTo` "BIT 1, C"
  it "decodes BIT 1, D" $ [0b11_001_011, 0b01_001_010] `decodesTo` "BIT 1, D"
  it "decodes BIT 1, E" $ [0b11_001_011, 0b01_001_011] `decodesTo` "BIT 1, E"
  it "decodes BIT 1, H" $ [0b11_001_011, 0b01_001_100] `decodesTo` "BIT 1, H"
  it "decodes BIT 1, L" $ [0b11_001_011, 0b01_001_101] `decodesTo` "BIT 1, L"
  it "decodes BIT 1, A" $ [0b11_001_011, 0b01_001_111] `decodesTo` "BIT 1, A"
  it "decodes BIT 1, (HL)" $ [0b11_001_011, 0b01_001_110] `decodesTo` "BIT 1, (HL)"
  it "decodes BIT 2, B" $ [0b11_001_011, 0b01_010_000] `decodesTo` "BIT 2, B"
  it "decodes BIT 2, C" $ [0b11_001_011, 0b01_010_001] `decodesTo` "BIT 2, C"
  it "decodes BIT 2, D" $ [0b11_001_011, 0b01_010_010] `decodesTo` "BIT 2, D"
  it "decodes BIT 2, E" $ [0b11_001_011, 0b01_010_011] `decodesTo` "BIT 2, E"
  it "decodes BIT 2, H" $ [0b11_001_011, 0b01_010_100] `decodesTo` "BIT 2, H"
  it "decodes BIT 2, L" $ [0b11_001_011, 0b01_010_101] `decodesTo` "BIT 2, L"
  it "decodes BIT 2, A" $ [0b11_001_011, 0b01_010_111] `decodesTo` "BIT 2, A"
  it "decodes BIT 2, (HL)" $ [0b11_001_011, 0b01_010_110] `decodesTo` "BIT 2, (HL)"
  it "decodes BIT 3, B" $ [0b11_001_011, 0b01_011_000] `decodesTo` "BIT 3, B"
  it "decodes BIT 3, C" $ [0b11_001_011, 0b01_011_001] `decodesTo` "BIT 3, C"
  it "decodes BIT 3, D" $ [0b11_001_011, 0b01_011_010] `decodesTo` "BIT 3, D"
  it "decodes BIT 3, E" $ [0b11_001_011, 0b01_011_011] `decodesTo` "BIT 3, E"
  it "decodes BIT 3, H" $ [0b11_001_011, 0b01_011_100] `decodesTo` "BIT 3, H"
  it "decodes BIT 3, L" $ [0b11_001_011, 0b01_011_101] `decodesTo` "BIT 3, L"
  it "decodes BIT 3, A" $ [0b11_001_011, 0b01_011_111] `decodesTo` "BIT 3, A"
  it "decodes BIT 3, (HL)" $ [0b11_001_011, 0b01_011_110] `decodesTo` "BIT 3, (HL)"
  it "decodes BIT 4, B" $ [0b11_001_011, 0b01_100_000] `decodesTo` "BIT 4, B"
  it "decodes BIT 4, C" $ [0b11_001_011, 0b01_100_001] `decodesTo` "BIT 4, C"
  it "decodes BIT 4, D" $ [0b11_001_011, 0b01_100_010] `decodesTo` "BIT 4, D"
  it "decodes BIT 4, E" $ [0b11_001_011, 0b01_100_011] `decodesTo` "BIT 4, E"
  it "decodes BIT 4, H" $ [0b11_001_011, 0b01_100_100] `decodesTo` "BIT 4, H"
  it "decodes BIT 4, L" $ [0b11_001_011, 0b01_100_101] `decodesTo` "BIT 4, L"
  it "decodes BIT 4, A" $ [0b11_001_011, 0b01_100_111] `decodesTo` "BIT 4, A"
  it "decodes BIT 4, (HL)" $ [0b11_001_011, 0b01_100_110] `decodesTo` "BIT 4, (HL)"
  it "decodes BIT 5, B" $ [0b11_001_011, 0b01_101_000] `decodesTo` "BIT 5, B"
  it "decodes BIT 5, C" $ [0b11_001_011, 0b01_101_001] `decodesTo` "BIT 5, C"
  it "decodes BIT 5, D" $ [0b11_001_011, 0b01_101_010] `decodesTo` "BIT 5, D"
  it "decodes BIT 5, E" $ [0b11_001_011, 0b01_101_011] `decodesTo` "BIT 5, E"
  it "decodes BIT 5, H" $ [0b11_001_011, 0b01_101_100] `decodesTo` "BIT 5, H"
  it "decodes BIT 5, L" $ [0b11_001_011, 0b01_101_101] `decodesTo` "BIT 5, L"
  it "decodes BIT 5, A" $ [0b11_001_011, 0b01_101_111] `decodesTo` "BIT 5, A"
  it "decodes BIT 5, (HL)" $ [0b11_001_011, 0b01_101_110] `decodesTo` "BIT 5, (HL)"
  it "decodes BIT 6, B" $ [0b11_001_011, 0b01_110_000] `decodesTo` "BIT 6, B"
  it "decodes BIT 6, C" $ [0b11_001_011, 0b01_110_001] `decodesTo` "BIT 6, C"
  it "decodes BIT 6, D" $ [0b11_001_011, 0b01_110_010] `decodesTo` "BIT 6, D"
  it "decodes BIT 6, E" $ [0b11_001_011, 0b01_110_011] `decodesTo` "BIT 6, E"
  it "decodes BIT 6, H" $ [0b11_001_011, 0b01_110_100] `decodesTo` "BIT 6, H"
  it "decodes BIT 6, L" $ [0b11_001_011, 0b01_110_101] `decodesTo` "BIT 6, L"
  it "decodes BIT 6, A" $ [0b11_001_011, 0b01_110_111] `decodesTo` "BIT 6, A"
  it "decodes BIT 6, (HL)" $ [0b11_001_011, 0b01_110_110] `decodesTo` "BIT 6, (HL)"
  it "decodes BIT 7, B" $ [0b11_001_011, 0b01_111_000] `decodesTo` "BIT 7, B"
  it "decodes BIT 7, C" $ [0b11_001_011, 0b01_111_001] `decodesTo` "BIT 7, C"
  it "decodes BIT 7, D" $ [0b11_001_011, 0b01_111_010] `decodesTo` "BIT 7, D"
  it "decodes BIT 7, E" $ [0b11_001_011, 0b01_111_011] `decodesTo` "BIT 7, E"
  it "decodes BIT 7, H" $ [0b11_001_011, 0b01_111_100] `decodesTo` "BIT 7, H"
  it "decodes BIT 7, L" $ [0b11_001_011, 0b01_111_101] `decodesTo` "BIT 7, L"
  it "decodes BIT 7, A" $ [0b11_001_011, 0b01_111_111] `decodesTo` "BIT 7, A"
  it "decodes BIT 7, (HL)" $ [0b11_001_011, 0b01_111_110] `decodesTo` "BIT 7, (HL)"
  it "decodes SET 0, B" $ [0b11_001_011, 0b11_000_000] `decodesTo` "SET 0, B"
  it "decodes SET 0, C" $ [0b11_001_011, 0b11_000_001] `decodesTo` "SET 0, C"
  it "decodes SET 0, D" $ [0b11_001_011, 0b11_000_010] `decodesTo` "SET 0, D"
  it "decodes SET 0, E" $ [0b11_001_011, 0b11_000_011] `decodesTo` "SET 0, E"
  it "decodes SET 0, H" $ [0b11_001_011, 0b11_000_100] `decodesTo` "SET 0, H"
  it "decodes SET 0, L" $ [0b11_001_011, 0b11_000_101] `decodesTo` "SET 0, L"
  it "decodes SET 0, A" $ [0b11_001_011, 0b11_000_111] `decodesTo` "SET 0, A"
  it "decodes SET 0, (HL)" $ [0b11_001_011, 0b11_000_110] `decodesTo` "SET 0, (HL)"
  it "decodes SET 1, B" $ [0b11_001_011, 0b11_001_000] `decodesTo` "SET 1, B"
  it "decodes SET 1, C" $ [0b11_001_011, 0b11_001_001] `decodesTo` "SET 1, C"
  it "decodes SET 1, D" $ [0b11_001_011, 0b11_001_010] `decodesTo` "SET 1, D"
  it "decodes SET 1, E" $ [0b11_001_011, 0b11_001_011] `decodesTo` "SET 1, E"
  it "decodes SET 1, H" $ [0b11_001_011, 0b11_001_100] `decodesTo` "SET 1, H"
  it "decodes SET 1, L" $ [0b11_001_011, 0b11_001_101] `decodesTo` "SET 1, L"
  it "decodes SET 1, A" $ [0b11_001_011, 0b11_001_111] `decodesTo` "SET 1, A"
  it "decodes SET 1, (HL)" $ [0b11_001_011, 0b11_001_110] `decodesTo` "SET 1, (HL)"
  it "decodes SET 2, B" $ [0b11_001_011, 0b11_010_000] `decodesTo` "SET 2, B"
  it "decodes SET 2, C" $ [0b11_001_011, 0b11_010_001] `decodesTo` "SET 2, C"
  it "decodes SET 2, D" $ [0b11_001_011, 0b11_010_010] `decodesTo` "SET 2, D"
  it "decodes SET 2, E" $ [0b11_001_011, 0b11_010_011] `decodesTo` "SET 2, E"
  it "decodes SET 2, H" $ [0b11_001_011, 0b11_010_100] `decodesTo` "SET 2, H"
  it "decodes SET 2, L" $ [0b11_001_011, 0b11_010_101] `decodesTo` "SET 2, L"
  it "decodes SET 2, A" $ [0b11_001_011, 0b11_010_111] `decodesTo` "SET 2, A"
  it "decodes SET 2, (HL)" $ [0b11_001_011, 0b11_010_110] `decodesTo` "SET 2, (HL)"
  it "decodes SET 3, B" $ [0b11_001_011, 0b11_011_000] `decodesTo` "SET 3, B"
  it "decodes SET 3, C" $ [0b11_001_011, 0b11_011_001] `decodesTo` "SET 3, C"
  it "decodes SET 3, D" $ [0b11_001_011, 0b11_011_010] `decodesTo` "SET 3, D"
  it "decodes SET 3, E" $ [0b11_001_011, 0b11_011_011] `decodesTo` "SET 3, E"
  it "decodes SET 3, H" $ [0b11_001_011, 0b11_011_100] `decodesTo` "SET 3, H"
  it "decodes SET 3, L" $ [0b11_001_011, 0b11_011_101] `decodesTo` "SET 3, L"
  it "decodes SET 3, A" $ [0b11_001_011, 0b11_011_111] `decodesTo` "SET 3, A"
  it "decodes SET 3, (HL)" $ [0b11_001_011, 0b11_011_110] `decodesTo` "SET 3, (HL)"
  it "decodes SET 4, B" $ [0b11_001_011, 0b11_100_000] `decodesTo` "SET 4, B"
  it "decodes SET 4, C" $ [0b11_001_011, 0b11_100_001] `decodesTo` "SET 4, C"
  it "decodes SET 4, D" $ [0b11_001_011, 0b11_100_010] `decodesTo` "SET 4, D"
  it "decodes SET 4, E" $ [0b11_001_011, 0b11_100_011] `decodesTo` "SET 4, E"
  it "decodes SET 4, H" $ [0b11_001_011, 0b11_100_100] `decodesTo` "SET 4, H"
  it "decodes SET 4, L" $ [0b11_001_011, 0b11_100_101] `decodesTo` "SET 4, L"
  it "decodes SET 4, A" $ [0b11_001_011, 0b11_100_111] `decodesTo` "SET 4, A"
  it "decodes SET 4, (HL)" $ [0b11_001_011, 0b11_100_110] `decodesTo` "SET 4, (HL)"
  it "decodes SET 5, B" $ [0b11_001_011, 0b11_101_000] `decodesTo` "SET 5, B"
  it "decodes SET 5, C" $ [0b11_001_011, 0b11_101_001] `decodesTo` "SET 5, C"
  it "decodes SET 5, D" $ [0b11_001_011, 0b11_101_010] `decodesTo` "SET 5, D"
  it "decodes SET 5, E" $ [0b11_001_011, 0b11_101_011] `decodesTo` "SET 5, E"
  it "decodes SET 5, H" $ [0b11_001_011, 0b11_101_100] `decodesTo` "SET 5, H"
  it "decodes SET 5, L" $ [0b11_001_011, 0b11_101_101] `decodesTo` "SET 5, L"
  it "decodes SET 5, A" $ [0b11_001_011, 0b11_101_111] `decodesTo` "SET 5, A"
  it "decodes SET 5, (HL)" $ [0b11_001_011, 0b11_101_110] `decodesTo` "SET 5, (HL)"
  it "decodes SET 6, B" $ [0b11_001_011, 0b11_110_000] `decodesTo` "SET 6, B"
  it "decodes SET 6, C" $ [0b11_001_011, 0b11_110_001] `decodesTo` "SET 6, C"
  it "decodes SET 6, D" $ [0b11_001_011, 0b11_110_010] `decodesTo` "SET 6, D"
  it "decodes SET 6, E" $ [0b11_001_011, 0b11_110_011] `decodesTo` "SET 6, E"
  it "decodes SET 6, H" $ [0b11_001_011, 0b11_110_100] `decodesTo` "SET 6, H"
  it "decodes SET 6, L" $ [0b11_001_011, 0b11_110_101] `decodesTo` "SET 6, L"
  it "decodes SET 6, A" $ [0b11_001_011, 0b11_110_111] `decodesTo` "SET 6, A"
  it "decodes SET 6, (HL)" $ [0b11_001_011, 0b11_110_110] `decodesTo` "SET 6, (HL)"
  it "decodes SET 7, B" $ [0b11_001_011, 0b11_111_000] `decodesTo` "SET 7, B"
  it "decodes SET 7, C" $ [0b11_001_011, 0b11_111_001] `decodesTo` "SET 7, C"
  it "decodes SET 7, D" $ [0b11_001_011, 0b11_111_010] `decodesTo` "SET 7, D"
  it "decodes SET 7, E" $ [0b11_001_011, 0b11_111_011] `decodesTo` "SET 7, E"
  it "decodes SET 7, H" $ [0b11_001_011, 0b11_111_100] `decodesTo` "SET 7, H"
  it "decodes SET 7, L" $ [0b11_001_011, 0b11_111_101] `decodesTo` "SET 7, L"
  it "decodes SET 7, A" $ [0b11_001_011, 0b11_111_111] `decodesTo` "SET 7, A"
  it "decodes SET 7, (HL)" $ [0b11_001_011, 0b11_111_110] `decodesTo` "SET 7, (HL)"
  it "decodes RES 0, B" $ [0b11_001_011, 0b10_000_000] `decodesTo` "RES 0, B"
  it "decodes RES 0, C" $ [0b11_001_011, 0b10_000_001] `decodesTo` "RES 0, C"
  it "decodes RES 0, D" $ [0b11_001_011, 0b10_000_010] `decodesTo` "RES 0, D"
  it "decodes RES 0, E" $ [0b11_001_011, 0b10_000_011] `decodesTo` "RES 0, E"
  it "decodes RES 0, H" $ [0b11_001_011, 0b10_000_100] `decodesTo` "RES 0, H"
  it "decodes RES 0, L" $ [0b11_001_011, 0b10_000_101] `decodesTo` "RES 0, L"
  it "decodes RES 0, A" $ [0b11_001_011, 0b10_000_111] `decodesTo` "RES 0, A"
  it "decodes RES 0, (HL)" $ [0b11_001_011, 0b10_000_110] `decodesTo` "RES 0, (HL)"
  it "decodes RES 1, B" $ [0b11_001_011, 0b10_001_000] `decodesTo` "RES 1, B"
  it "decodes RES 1, C" $ [0b11_001_011, 0b10_001_001] `decodesTo` "RES 1, C"
  it "decodes RES 1, D" $ [0b11_001_011, 0b10_001_010] `decodesTo` "RES 1, D"
  it "decodes RES 1, E" $ [0b11_001_011, 0b10_001_011] `decodesTo` "RES 1, E"
  it "decodes RES 1, H" $ [0b11_001_011, 0b10_001_100] `decodesTo` "RES 1, H"
  it "decodes RES 1, L" $ [0b11_001_011, 0b10_001_101] `decodesTo` "RES 1, L"
  it "decodes RES 1, A" $ [0b11_001_011, 0b10_001_111] `decodesTo` "RES 1, A"
  it "decodes RES 1, (HL)" $ [0b11_001_011, 0b10_001_110] `decodesTo` "RES 1, (HL)"
  it "decodes RES 2, B" $ [0b11_001_011, 0b10_010_000] `decodesTo` "RES 2, B"
  it "decodes RES 2, C" $ [0b11_001_011, 0b10_010_001] `decodesTo` "RES 2, C"
  it "decodes RES 2, D" $ [0b11_001_011, 0b10_010_010] `decodesTo` "RES 2, D"
  it "decodes RES 2, E" $ [0b11_001_011, 0b10_010_011] `decodesTo` "RES 2, E"
  it "decodes RES 2, H" $ [0b11_001_011, 0b10_010_100] `decodesTo` "RES 2, H"
  it "decodes RES 2, L" $ [0b11_001_011, 0b10_010_101] `decodesTo` "RES 2, L"
  it "decodes RES 2, A" $ [0b11_001_011, 0b10_010_111] `decodesTo` "RES 2, A"
  it "decodes RES 2, (HL)" $ [0b11_001_011, 0b10_010_110] `decodesTo` "RES 2, (HL)"
  it "decodes RES 3, B" $ [0b11_001_011, 0b10_011_000] `decodesTo` "RES 3, B"
  it "decodes RES 3, C" $ [0b11_001_011, 0b10_011_001] `decodesTo` "RES 3, C"
  it "decodes RES 3, D" $ [0b11_001_011, 0b10_011_010] `decodesTo` "RES 3, D"
  it "decodes RES 3, E" $ [0b11_001_011, 0b10_011_011] `decodesTo` "RES 3, E"
  it "decodes RES 3, H" $ [0b11_001_011, 0b10_011_100] `decodesTo` "RES 3, H"
  it "decodes RES 3, L" $ [0b11_001_011, 0b10_011_101] `decodesTo` "RES 3, L"
  it "decodes RES 3, A" $ [0b11_001_011, 0b10_011_111] `decodesTo` "RES 3, A"
  it "decodes RES 3, (HL)" $ [0b11_001_011, 0b10_011_110] `decodesTo` "RES 3, (HL)"
  it "decodes RES 4, B" $ [0b11_001_011, 0b10_100_000] `decodesTo` "RES 4, B"
  it "decodes RES 4, C" $ [0b11_001_011, 0b10_100_001] `decodesTo` "RES 4, C"
  it "decodes RES 4, D" $ [0b11_001_011, 0b10_100_010] `decodesTo` "RES 4, D"
  it "decodes RES 4, E" $ [0b11_001_011, 0b10_100_011] `decodesTo` "RES 4, E"
  it "decodes RES 4, H" $ [0b11_001_011, 0b10_100_100] `decodesTo` "RES 4, H"
  it "decodes RES 4, L" $ [0b11_001_011, 0b10_100_101] `decodesTo` "RES 4, L"
  it "decodes RES 4, A" $ [0b11_001_011, 0b10_100_111] `decodesTo` "RES 4, A"
  it "decodes RES 4, (HL)" $ [0b11_001_011, 0b10_100_110] `decodesTo` "RES 4, (HL)"
  it "decodes RES 5, B" $ [0b11_001_011, 0b10_101_000] `decodesTo` "RES 5, B"
  it "decodes RES 5, C" $ [0b11_001_011, 0b10_101_001] `decodesTo` "RES 5, C"
  it "decodes RES 5, D" $ [0b11_001_011, 0b10_101_010] `decodesTo` "RES 5, D"
  it "decodes RES 5, E" $ [0b11_001_011, 0b10_101_011] `decodesTo` "RES 5, E"
  it "decodes RES 5, H" $ [0b11_001_011, 0b10_101_100] `decodesTo` "RES 5, H"
  it "decodes RES 5, L" $ [0b11_001_011, 0b10_101_101] `decodesTo` "RES 5, L"
  it "decodes RES 5, A" $ [0b11_001_011, 0b10_101_111] `decodesTo` "RES 5, A"
  it "decodes RES 5, (HL)" $ [0b11_001_011, 0b10_101_110] `decodesTo` "RES 5, (HL)"
  it "decodes RES 6, B" $ [0b11_001_011, 0b10_110_000] `decodesTo` "RES 6, B"
  it "decodes RES 6, C" $ [0b11_001_011, 0b10_110_001] `decodesTo` "RES 6, C"
  it "decodes RES 6, D" $ [0b11_001_011, 0b10_110_010] `decodesTo` "RES 6, D"
  it "decodes RES 6, E" $ [0b11_001_011, 0b10_110_011] `decodesTo` "RES 6, E"
  it "decodes RES 6, H" $ [0b11_001_011, 0b10_110_100] `decodesTo` "RES 6, H"
  it "decodes RES 6, L" $ [0b11_001_011, 0b10_110_101] `decodesTo` "RES 6, L"
  it "decodes RES 6, A" $ [0b11_001_011, 0b10_110_111] `decodesTo` "RES 6, A"
  it "decodes RES 6, (HL)" $ [0b11_001_011, 0b10_110_110] `decodesTo` "RES 6, (HL)"
  it "decodes RES 7, B" $ [0b11_001_011, 0b10_111_000] `decodesTo` "RES 7, B"
  it "decodes RES 7, C" $ [0b11_001_011, 0b10_111_001] `decodesTo` "RES 7, C"
  it "decodes RES 7, D" $ [0b11_001_011, 0b10_111_010] `decodesTo` "RES 7, D"
  it "decodes RES 7, E" $ [0b11_001_011, 0b10_111_011] `decodesTo` "RES 7, E"
  it "decodes RES 7, H" $ [0b11_001_011, 0b10_111_100] `decodesTo` "RES 7, H"
  it "decodes RES 7, L" $ [0b11_001_011, 0b10_111_101] `decodesTo` "RES 7, L"
  it "decodes RES 7, A" $ [0b11_001_011, 0b10_111_111] `decodesTo` "RES 7, A"
  it "decodes RES 7, (HL)" $ [0b11_001_011, 0b10_111_110] `decodesTo` "RES 7, (HL)"
  it "decodes JP n" $ [0b11_000_011, 0x32, 0x42] `decodesTo` "JP 4232"
  it "decodes JP NZ, n" $ [0b11_000_010, 0x32, 0x42] `decodesTo` "JP NZ, 4232"
  it "decodes JP Z, n" $ [0b11_001_010, 0x32, 0x42] `decodesTo` "JP Z, 4232"
  it "decodes JP NC, n" $ [0b11_010_010, 0x32, 0x42] `decodesTo` "JP NC, 4232"
  it "decodes JP C, n" $ [0b11_011_010, 0x32, 0x42] `decodesTo` "JP C, 4232"
  it "decodes JP (HL)" $ [0b11_101_001] `decodesTo` "JP (HL)"
  it "decodes JR n" $ [0b00_011_000, 0x42] `decodesTo` "JR 42"
  it "decodes JR NZ, n" $ [0b00_100_000, 0x42] `decodesTo` "JR NZ, 42"
  it "decodes JR Z, n" $ [0b00_101_000, 0x42] `decodesTo` "JR Z, 42"
  it "decodes JR NC, n" $ [0b00_110_000, 0x42] `decodesTo` "JR NC, 42"
  it "decodes JR C, n" $ [0b00_111_000, 0x42] `decodesTo` "JR C, 42"
  it "decodes CALL n" $ [0b11_001_101, 0x32, 0x42] `decodesTo` "CALL 4232"
  it "decodes CALL NZ, n" $ [0b11_000_100, 0x32, 0x42] `decodesTo` "CALL NZ, 4232"
  it "decodes CALL Z, n" $ [0b11_001_100, 0x32, 0x42] `decodesTo` "CALL Z, 4232"
  it "decodes CALL NC, n" $ [0b11_010_100, 0x32, 0x42] `decodesTo` "CALL NC, 4232"
  it "decodes CALL C, n" $ [0b11_011_100, 0x32, 0x42] `decodesTo` "CALL C, 4232"
  it "decodes RET" $ [0b11_001_001] `decodesTo` "RET"
  it "decodes RET NZ" $ [0b11_000_000] `decodesTo` "RET NZ"
  it "decodes RET Z" $ [0b11_001_000] `decodesTo` "RET Z"
  it "decodes RET NC" $ [0b11_010_000] `decodesTo` "RET NC"
  it "decodes RET C" $ [0b11_011_000] `decodesTo` "RET C"
  it "decodes RETI" $ [0b11_011_001] `decodesTo` "RETI"
  it "decodes RST n" $ [0b11_000_111] `decodesTo` "RST 0"
  it "decodes RST n" $ [0b11_001_111] `decodesTo` "RST 1"
  it "decodes RST n" $ [0b11_010_111] `decodesTo` "RST 2"
  it "decodes RST n" $ [0b11_011_111] `decodesTo` "RST 3"
  it "decodes RST n" $ [0b11_100_111] `decodesTo` "RST 4"
  it "decodes RST n" $ [0b11_101_111] `decodesTo` "RST 5"
  it "decodes RST n" $ [0b11_110_111] `decodesTo` "RST 6"
  it "decodes RST n" $ [0b11_111_111] `decodesTo` "RST 7"
  it "decodes DAA" $ [0b00_100_111] `decodesTo` "DAA"
  it "decodes CPL" $ [0b00_101_111] `decodesTo` "CPL"
  it "decodes NOP" $ [0b00_000_000] `decodesTo` "NOP"
  it "decodes CCF" $ [0b00_111_111] `decodesTo` "CCF"
  it "decodes SCF" $ [0b00_110_111] `decodesTo` "SCF"
  it "decodes DI" $ [0b11_110_011] `decodesTo` "DI"
  it "decodes EI" $ [0b11_111_011] `decodesTo` "EI"
  it "decodes HALT" $ [0b01_110_110] `decodesTo` "HALT"
  it "decodes STOP" $ [0b00_010_000, 0] `decodesTo` "STOP"
  it "does not decode invalid STOP" $ [0b00_010_000, 1] `decodesTo` ".data 10"
  it "does not decode invalid instruction 11_100 100" $ [0b11_100_100] `decodesTo` ".data E4"
  it "does not decode invalid instruction 11_101 100" $ [0b11_101_100] `decodesTo` ".data EC"
  it "does not decode invalid instruction 11_110 100" $ [0b11_110_100] `decodesTo` ".data F4"
  it "does not decode invalid instruction 11_111 100" $ [0b11_111_100] `decodesTo` ".data FC"
  it "does not decode invalid instruction 11_011 101" $ [0b11_011_101] `decodesTo` ".data DD"
  it "does not decode invalid instruction 11_101 101" $ [0b11_101_101] `decodesTo` ".data ED"
  it "does not decode invalid instruction 11_111 101" $ [0b11_111_101] `decodesTo` ".data FD"
