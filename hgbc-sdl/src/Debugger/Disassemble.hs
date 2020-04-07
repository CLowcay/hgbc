{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Debugger.Disassemble
  ( Field(..)
  , LongAddress(..)
  , disassembleFrom
  )
where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString.Short
import           Data.Int
import           Data.String
import           Data.Word
import           Machine.GBC.CPU.Decode
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Memory
import           Machine.GBC.Util
import qualified Data.ByteString.Short         as SB
import qualified Data.Map                      as M
import qualified Data.Text                     as T

data Field = Field !ShortByteString !T.Text
  deriving (Eq, Ord, Show)

data NextAction
  = Continue
  | Jump !Word16
  | JumpRel !Int8
  | Fork !Word16
  | ForkRel !Int8
  | Stop
  deriving (Eq, Ord, Show)

data LongAddress
  = LongAddress {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
  deriving (Eq, Ord, Show)

data DisassemblyState = DisassemblyState {
    address :: {-# UNPACK #-} !Word16
  , bytes   :: ![Word8]
  , memory  :: !Memory
}

currentAddressLong :: StateT DisassemblyState IO LongAddress
currentAddressLong = do
  s    <- get
  bank <- liftIO (runReaderT (getBank (address s)) (memory s))
  pure (LongAddress bank (address s))

currentInstructionBytes :: StateT DisassemblyState IO SB.ShortByteString
currentInstructionBytes = do
  s <- get
  put (s { bytes = [] })
  pure (SB.pack (reverse (bytes s)))

modifyAddress :: (Word16 -> Word16) -> StateT DisassemblyState IO ()
modifyAddress f = do
  s <- get
  put (s { address = f (address s) })

disassembleFrom :: Memory -> Word16 -> IO (M.Map LongAddress Field)
disassembleFrom memory0 = innerDisassemble mempty
 where
  innerDisassemble disassembly startAddress =
    evalStateT (go disassembly) (DisassemblyState startAddress [] memory0)

  go !accum = do
    addrLong <- currentAddressLong
    if addrLong `M.member` accum
      then pure accum
      else do
        (action, r) <- fetchAndExecute
        bs          <- currentInstructionBytes
        let accum' = M.insert addrLong (Field bs r) accum
        case action of
          Continue   -> go accum'
          Jump    nn -> modifyAddress (const nn) >> go accum'
          JumpRel i  -> modifyAddress (+ fromIntegral i) >> go accum'
          Fork    nn -> liftIO (innerDisassemble accum' nn) >>= go
          ForkRel i  -> do
            currentAddress <- gets address
            go =<< liftIO (innerDisassemble accum' (currentAddress + fromIntegral i))
          Stop -> pure accum'

formatR8 :: Register8 -> T.Text
formatR8 RegA = "A"
formatR8 RegB = "B"
formatR8 RegC = "C"
formatR8 RegD = "D"
formatR8 RegE = "E"
formatR8 RegH = "H"
formatR8 RegL = "L"

formatR16 :: Register16 -> T.Text
formatR16 RegBC = "BC"
formatR16 RegDE = "DE"
formatR16 RegHL = "HL"
formatR16 RegSP = "SP"

formatRpp :: RegisterPushPop -> T.Text
formatRpp PushPopAF = "AF"
formatRpp PushPopBC = "BC"
formatRpp PushPopDE = "DE"
formatRpp PushPopHL = "HL"

formatCC :: ConditionCode -> T.Text
formatCC CondC  = "C"
formatCC CondNC = "NC"
formatCC CondZ  = "Z"
formatCC CondNZ = "NZ"

instance MonadFetch (StateT DisassemblyState IO) where
  nextByte = do
    s    <- get
    byte <- liftIO (runReaderT (readByte (address s)) (memory s))
    put (s { address = address s + 1, bytes = byte : bytes s })
    pure byte

instance MonadGMBZ80 (StateT DisassemblyState IO) where
  type ExecuteResult (StateT DisassemblyState IO) = (NextAction, T.Text)
  ldrr r r' = pure (Continue, "LD " <> formatR8 r <> ", " <> formatR8 r')
  ldrn r n = pure (Continue, "LD " <> formatR8 r <> ", " <> fromString (formatHex n))
  ldrHL r = pure (Continue, "LD " <> formatR8 r <> ", (HL)")
  ldHLr r = pure (Continue, "LD (HL), " <> formatR8 r)
  ldHLn n = pure (Continue, "LD (HL), " <> fromString (formatHex n))
  ldaBC = pure (Continue, "LD A, (BC)")
  ldaDE = pure (Continue, "LD A, (DE)")
  ldaC  = pure (Continue, "LD A, (C)")
  ldCa  = pure (Continue, "LD (C), A")
  ldan n = pure (Continue, "LD A, (FF" <> fromString (formatHex n) <> ")")
  ldna n = pure (Continue, "LD (FF" <> fromString (formatHex n) <> "), A")
  ldann nn = pure (Continue, "LD A, (" <> fromString (formatHex nn) <> ")")
  ldnna nn = pure (Continue, "LD (" <> fromString (formatHex nn) <> "), A")
  ldaHLI = pure (Continue, "LD A, (HLI)")
  ldaHLD = pure (Continue, "LD A, (HLD)")
  ldBCa  = pure (Continue, "LD (BC), A")
  ldDEa  = pure (Continue, "LD (DE), A")
  ldHLIa = pure (Continue, "LD (HLI), A")
  ldHLDa = pure (Continue, "LD (HLD), A")
  ldddnn dd nn = pure (Continue, "LD " <> formatR16 dd <> ", " <> fromString (formatHex nn))
  ldSPHL = pure (Continue, "LD SP, HL")
  push qq = pure (Continue, "PUSH " <> formatRpp qq)
  pop qq = pure (Continue, "POP " <> formatRpp qq)
  ldhl i = pure (Continue, "LDHL SP, " <> fromString (formatHex i))
  ldnnSP nn = pure (Continue, "LD (" <> fromString (formatHex nn) <> "), SP")
  addr r = pure (Continue, "ADD A, " <> formatR8 r)
  addn w = pure (Continue, "ADD A, " <> fromString (formatHex w))
  addhl = pure (Continue, "ADD A, (HL)")
  adcr r = pure (Continue, "ADC A, " <> formatR8 r)
  adcn i = pure (Continue, "ADC A, " <> fromString (formatHex i))
  adchl = pure (Continue, "ADC A, (HL)")
  subr r = pure (Continue, "SUB A, " <> formatR8 r)
  subn i = pure (Continue, "SUB A, " <> fromString (formatHex i))
  subhl = pure (Continue, "SUB A, (HL)")
  sbcr r = pure (Continue, "SBC A, " <> formatR8 r)
  sbcn i = pure (Continue, "SBC A, " <> fromString (formatHex i))
  sbchl = pure (Continue, "SBC A, (HL)")
  andr r = pure (Continue, "AND A, " <> formatR8 r)
  andn i = pure (Continue, "AND A, " <> fromString (formatHex i))
  andhl = pure (Continue, "AND A, (HL)")
  orr r = pure (Continue, "OR A, " <> formatR8 r)
  orn i = pure (Continue, "OR A, " <> fromString (formatHex i))
  orhl = pure (Continue, "OR A, (HL)")
  xorr r = pure (Continue, "XOR A, " <> formatR8 r)
  xorn i = pure (Continue, "XOR A, " <> fromString (formatHex i))
  xorhl = pure (Continue, "XOR A, (HL)")
  cpr r = pure (Continue, "CP A, " <> formatR8 r)
  cpn i = pure (Continue, "CP A, " <> fromString (formatHex i))
  cphl = pure (Continue, "CP A, (HL)")
  incr r = pure (Continue, "INC " <> formatR8 r)
  inchl = pure (Continue, "INC (HL)")
  decr r = pure (Continue, "DEC " <> formatR8 r)
  dechl = pure (Continue, "DEC (HL)")
  addhlss ss = pure (Continue, "ADD HL, " <> formatR16 ss)
  addSP i = pure (Continue, "ADD SP, " <> fromString (formatHex i))
  incss ss = pure (Continue, "INC " <> formatR16 ss)
  decss ss = pure (Continue, "DEC " <> formatR16 ss)
  rlca = pure (Continue, "RLCA")
  rla  = pure (Continue, "RLA")
  rrca = pure (Continue, "RRCA")
  rra  = pure (Continue, "RRA")
  rlcr r = pure (Continue, "RLC " <> formatR8 r)
  rlchl = pure (Continue, "RLC (HL)")
  rlr r = pure (Continue, "RL " <> formatR8 r)
  rlhl = pure (Continue, "RL (HL)")
  rrcr r = pure (Continue, "RRC " <> formatR8 r)
  rrchl = pure (Continue, "RRC (HL)")
  rrr r = pure (Continue, "RR " <> formatR8 r)
  rrhl = pure (Continue, "RR (HL)")
  slar r = pure (Continue, "SLA " <> formatR8 r)
  slahl = pure (Continue, "SLA (HL)")
  srar r = pure (Continue, "SRA " <> formatR8 r)
  srahl = pure (Continue, "SRA (HL)")
  srlr r = pure (Continue, "SRL " <> formatR8 r)
  srlhl = pure (Continue, "SRL (HL)")
  swapr r = pure (Continue, "SWAP " <> formatR8 r)
  swaphl = pure (Continue, "SWAP (HL)")
  bitr r i = pure (Continue, "BIT " <> fromString (show i) <> ", " <> formatR8 r)
  bithl i = pure (Continue, "BIT " <> fromString (show i) <> ", (HL)")
  setr r i = pure (Continue, "SET " <> fromString (show i) <> ", " <> formatR8 r)
  sethl i = pure (Continue, "SET " <> fromString (show i) <> ", (HL)")
  resr r i = pure (Continue, "RES " <> fromString (show i) <> ", " <> formatR8 r)
  reshl i = pure (Continue, "RES " <> fromString (show i) <> ", (HL)")
  jpnn nn = pure (Jump nn, "JP " <> fromString (formatHex nn))
  jphl = pure (Stop, "JP (HL)")
  jpccnn cc nn = pure (Fork nn, "JP " <> formatCC cc <> ", " <> fromString (formatHex nn))
  jr i = pure (JumpRel i, "JR " <> fromString (formatHex i))
  jrcc cc i = pure (ForkRel i, "JR " <> formatCC cc <> ", " <> fromString (formatHex i))
  call nn = pure (Fork nn, "CALL " <> fromString (formatHex nn))
  callcc cc nn = pure (Fork nn, "CALL " <> formatCC cc <> ", " <> fromString (formatHex nn))
  ret  = pure (Stop, "RET")
  reti = pure (Stop, "RETI")
  retcc cc = pure (Continue, "RET " <> formatCC cc)
  rst i = pure (Jump (8 * fromIntegral i), "RST " <> fromString (show i))
  daa  = pure (Continue, "DAA")
  cpl  = pure (Continue, "CPL")
  nop  = pure (Continue, "NOP")
  ccf  = pure (Continue, "CCF")
  scf  = pure (Continue, "SCF")
  di   = pure (Continue, "DI")
  ei   = pure (Continue, "EI")
  halt = pure (Continue, "HALT")
  stop = pure (Continue, "STOP")
  invalid b = pure (Stop, "INVALID " <> fromString (formatHex b))
