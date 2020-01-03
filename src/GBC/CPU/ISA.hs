{-# LANGUAGE TypeFamilyDependencies #-}

module GBC.CPU.ISA
  ( RegisterR(..)
  , RegisterDD
  , RegisterSS(..)
  , RegisterQQ(..)
  , ConditionCode(..)
  , ExecuteUnit(..)
  , PureDisassemble(..)
  )
where

import           Common
import           Data.Int
import           Data.Word

-- | An 8-bit register
data RegisterR = RegA | RegB | RegC | RegD | RegE | RegH | RegL deriving (Eq, Ord, Show, Bounded, Enum)

-- | A 16-bit register.
type RegisterDD = RegisterSS
data RegisterSS = RegSP | RegBC | RegDE | RegHL deriving (Eq, Ord, Show, Bounded, Enum)

-- | A 16-bit register (for push and pop instructions).
data RegisterQQ = PushPopAF | PushPopBC | PushPopDE | PushPopHL deriving (Eq, Ord, Show, Bounded, Enum)

-- | A condition.
data ConditionCode = CondNZ | CondZ | CondNC | CondC deriving (Eq, Ord, Show, Bounded, Enum)

class ExecuteUnit p where
  type ExecuteResult p = r | r -> p
  ldrr    :: p -> RegisterR -> RegisterR -> ExecuteResult p
  ldrn    :: p -> RegisterR -> Word8 -> ExecuteResult p
  ldrHL   :: p -> RegisterR -> ExecuteResult p
  ldHLr   :: p -> RegisterR -> ExecuteResult p
  ldHLn   :: p -> Word8 -> ExecuteResult p
  ldaBC   :: p -> ExecuteResult p
  ldaDE   :: p -> ExecuteResult p
  ldaC    :: p -> ExecuteResult p
  ldCa    :: p -> ExecuteResult p
  ldan    :: p -> Word8 -> ExecuteResult p
  ldna    :: p -> Word8 -> ExecuteResult p
  ldann   :: p -> Word16 -> ExecuteResult p
  ldnna   :: p -> Word16 -> ExecuteResult p
  ldaHLI  :: p -> ExecuteResult p
  ldaHLD  :: p -> ExecuteResult p
  ldBCa   :: p -> ExecuteResult p
  ldDEa   :: p -> ExecuteResult p
  ldHLIa  :: p -> ExecuteResult p
  ldHLDa  :: p -> ExecuteResult p
  ldddnn  :: p -> RegisterDD -> Word16 -> ExecuteResult p
  ldSPHL  :: p -> ExecuteResult p
  push    :: p -> RegisterQQ -> ExecuteResult p
  pop     :: p -> RegisterQQ -> ExecuteResult p
  ldhl    :: p -> Int8 -> ExecuteResult p
  ldnnSP  :: p -> Word16 -> ExecuteResult p
  addr    :: p -> RegisterR -> ExecuteResult p
  addn    :: p -> Word8 -> ExecuteResult p
  addhl   :: p -> ExecuteResult p
  adcr    :: p -> RegisterR -> ExecuteResult p
  adcn    :: p -> Word8 -> ExecuteResult p
  adchl   :: p -> ExecuteResult p
  subr    :: p -> RegisterR -> ExecuteResult p
  subn    :: p -> Word8 -> ExecuteResult p
  subhl   :: p -> ExecuteResult p
  sbcr    :: p -> RegisterR -> ExecuteResult p
  sbcn    :: p -> Word8 -> ExecuteResult p
  sbchl   :: p -> ExecuteResult p
  andr    :: p -> RegisterR -> ExecuteResult p
  andn    :: p -> Word8 -> ExecuteResult p
  andhl   :: p -> ExecuteResult p
  orr     :: p -> RegisterR -> ExecuteResult p
  orn     :: p -> Word8 -> ExecuteResult p
  orhl    :: p -> ExecuteResult p
  xorr    :: p -> RegisterR -> ExecuteResult p
  xorn    :: p -> Word8 -> ExecuteResult p
  xorhl   :: p -> ExecuteResult p
  cpr     :: p -> RegisterR -> ExecuteResult p
  cpn     :: p -> Word8 -> ExecuteResult p
  cphl    :: p -> ExecuteResult p
  incr    :: p -> RegisterR -> ExecuteResult p
  inchl   :: p -> ExecuteResult p
  decr    :: p -> RegisterR -> ExecuteResult p
  dechl   :: p -> ExecuteResult p
  addhlss :: p -> RegisterSS -> ExecuteResult p
  addSP   :: p -> Int8 -> ExecuteResult p
  incss   :: p -> RegisterSS -> ExecuteResult p
  decss   :: p -> RegisterSS -> ExecuteResult p
  rlca    :: p -> ExecuteResult p
  rla     :: p -> ExecuteResult p
  rrca    :: p -> ExecuteResult p
  rra     :: p -> ExecuteResult p
  rlcr    :: p -> RegisterR -> ExecuteResult p
  rlchl   :: p -> ExecuteResult p
  rlr     :: p -> RegisterR -> ExecuteResult p
  rlhl    :: p -> ExecuteResult p
  rrcr    :: p -> RegisterR -> ExecuteResult p
  rrchl   :: p -> ExecuteResult p
  rrr     :: p -> RegisterR -> ExecuteResult p
  rrhl    :: p -> ExecuteResult p
  slar    :: p -> RegisterR -> ExecuteResult p
  slahl   :: p -> ExecuteResult p
  srar    :: p -> RegisterR -> ExecuteResult p
  srahl   :: p -> ExecuteResult p
  srlr    :: p -> RegisterR -> ExecuteResult p
  srlhl   :: p -> ExecuteResult p
  swapr   :: p -> RegisterR -> ExecuteResult p
  swaphl  :: p -> ExecuteResult p
  bitr    :: p -> RegisterR -> Word8 -> ExecuteResult p
  bithl   :: p -> Word8 -> ExecuteResult p
  setr    :: p -> RegisterR -> Word8 -> ExecuteResult p
  sethl   :: p -> Word8 -> ExecuteResult p
  resr    :: p -> RegisterR -> Word8 -> ExecuteResult p
  reshl   :: p -> Word8 -> ExecuteResult p
  jpnn    :: p -> Word16 -> ExecuteResult p
  jphl    :: p -> ExecuteResult p
  jpccnn  :: p -> ConditionCode -> Word16 -> ExecuteResult p
  jr      :: p -> Int8 -> ExecuteResult p
  jrcc    :: p -> ConditionCode -> Int8 -> ExecuteResult p
  call    :: p -> Word16 -> ExecuteResult p
  callcc  :: p -> ConditionCode -> Word16 -> ExecuteResult p
  ret     :: p -> ExecuteResult p
  reti    :: p -> ExecuteResult p
  retcc   :: p -> ConditionCode -> ExecuteResult p
  rst     :: p -> Word8 -> ExecuteResult p
  daa     :: p -> ExecuteResult p
  cpl     :: p -> ExecuteResult p
  nop     :: p -> ExecuteResult p
  ccf     :: p -> ExecuteResult p
  scf     :: p -> ExecuteResult p
  di      :: p -> ExecuteResult p
  ei      :: p -> ExecuteResult p
  halt    :: p -> ExecuteResult p
  stop    :: p -> ExecuteResult p
  invalid :: p -> Word8 -> ExecuteResult p

data PureDisassemble  = PureDisassemble {
  codeMap :: Word16 -> Maybe String
}

instance Format RegisterR where
  format RegA = "A"
  format RegB = "B"
  format RegC = "C"
  format RegD = "D"
  format RegE = "E"
  format RegH = "H"
  format RegL = "L"

instance Format RegisterSS where
  format RegSP = "SP"
  format RegBC = "BC"
  format RegDE = "DE"
  format RegHL = "HL"

instance Format RegisterQQ where
  format PushPopAF = "AF"
  format PushPopBC = "BC"
  format PushPopDE = "DE"
  format PushPopHL = "HL"

instance Format ConditionCode where
  format CondNZ = "NZ"
  format CondZ  = "Z"
  format CondNC = "NC"
  format CondC  = "C"

lookupAddress :: PureDisassemble -> Word16 -> String
lookupAddress d = maybe "" ('@' :) . codeMap d

instance ExecuteUnit PureDisassemble where
  type ExecuteResult PureDisassemble = String
  ldrr _ r r' = "LD " <> format r <> ", " <> format r'
  ldrn _ r n = "LD " <> format r <> ", " <> formatHex n
  ldrHL _ r = "LD " <> format r <> ", (HL)"
  ldHLr _ r = "LD (HL), " <> format r
  ldHLn _ w = "LD (HL), " <> formatHex w
  ldaBC _ = "LD A, (BC)"
  ldaDE _ = "LD A, (DE)"
  ldaC _ = "LD A, (C)"
  ldCa _ = "LD (C), A"
  ldan d w = "LD A, (FF" <> formatHex w <> lookupAddress d (0xFF00 + fromIntegral w) <> ")"
  ldna d w = "LD (FF" <> formatHex w <> lookupAddress d (0xFF00 + fromIntegral w) <> "), A"
  ldann d ww = "LD A, (" <> formatHex ww <> lookupAddress d ww <> ")"
  ldnna d ww = "LD (" <> formatHex ww <> lookupAddress d ww <> "), A"
  ldaHLI _ = "LD A, (HLI)"
  ldaHLD _ = "LD A, (HLD)"
  ldBCa _ = "LD (BC), A"
  ldDEa _ = "LD (DE), A"
  ldHLIa _ = "LD (HLI), A"
  ldHLDa _ = "LD (HLD), A"
  ldddnn _ dd ww = "LD " <> format dd <> ", " <> formatHex ww
  ldSPHL _ = "LD SP, HL"
  push _ qq = "PUSH " <> format qq
  pop _ qq = "POP " <> format qq
  ldhl _ i = "LDHL SP, " <> formatHex i
  ldnnSP d ww = "LD (" <> formatHex ww <> lookupAddress d ww <> "), SP"
  addr _ r = "ADD A, " <> format r
  addn _ w = "ADD A, " <> formatHex w
  addhl _ = "ADD A, (HL)"
  adcr _ r = "ADC A, " <> format r
  adcn _ i = "ADC A, " <> formatHex i
  adchl _ = "ADC A, (HL)"
  subr _ r = "SUB A, " <> format r
  subn _ i = "SUB A, " <> formatHex i
  subhl _ = "SUB A, (HL)"
  sbcr _ r = "SBC A, " <> format r
  sbcn _ i = "SBC A, " <> formatHex i
  sbchl _ = "SBC A, (HL)"
  andr _ r = "AND A, " <> format r
  andn _ i = "AND A, " <> formatHex i
  andhl _ = "AND A, (HL)"
  orr _ r = "OR A, " <> format r
  orn _ i = "OR A, " <> formatHex i
  orhl _ = "OR A, (HL)"
  xorr _ r = "XOR A, " <> format r
  xorn _ i = "XOR A, " <> formatHex i
  xorhl _ = "XOR A, (HL)"
  cpr _ r = "CP A, " <> format r
  cpn _ i = "CP A, " <> formatHex i
  cphl _ = "CP A, (HL)"
  incr _ r = "INC " <> format r
  inchl _ = "INC (HL)"
  decr _ r = "DEC " <> format r
  dechl _ = "DEC (HL)"
  addhlss _ ss = "ADD HL, " <> format ss
  addSP _ i = "ADD SP, " <> formatHex i
  incss _ ss = "INC " <> format ss
  decss _ ss = "DEC " <> format ss
  rlca _ = "RLCA"
  rla _ = "RLA"
  rrca _ = "RRCA"
  rra _ = "RRA"
  rlcr _ r = "RLC " <> format r
  rlchl _ = "RLC (HL)"
  rlr _ r = "RL " <> format r
  rlhl _ = "RL (HL)"
  rrcr _ r = "RRC " <> format r
  rrchl _ = "RRC (HL)"
  rrr _ r = "RR " <> format r
  rrhl _ = "RR (HL)"
  slar _ r = "SLA " <> format r
  slahl _ = "SLA (HL)"
  srar _ r = "SRA " <> format r
  srahl _ = "SRA (HL)"
  srlr _ r = "SRL " <> format r
  srlhl _ = "SRL (HL)"
  swapr _ r = "SWAP " <> format r
  swaphl _ = "SWAP (HL)"
  bitr _ r i = "BIT " <> show i <> ", " <> format r
  bithl _ i = "BIT " <> show i <> ", (HL)"
  setr _ r i = "SET " <> show i <> ", " <> format r
  sethl _ i = "SET " <> show i <> ", (HL)"
  resr _ r i = "RES " <> show i <> ", " <> format r
  reshl _ i = "RES " <> show i <> ", (HL)"
  jpnn d ww = "JP " <> formatHex ww <> lookupAddress d ww
  jphl _ = "JP (HL)"
  jpccnn d cc ww = "JP " <> format cc <> ", " <> formatHex ww <> lookupAddress d ww
  jr _ i = "JR " <> formatHex i
  jrcc _ cc i = "JR " <> format cc <> ", " <> formatHex i
  call d ww = "CALL " <> formatHex ww <> lookupAddress d ww
  callcc d cc ww = "CALL " <> format cc <> ", " <> formatHex ww <> lookupAddress d ww
  ret _ = "RET"
  reti _ = "RETI"
  retcc _ cc = "RET " <> format cc
  rst _ i = "RST " <> show i
  daa _ = "DAA"
  cpl _ = "CPL"
  nop _ = "NOP"
  ccf _ = "CCF"
  scf _ = "SCF"
  di _ = "DI"
  ei _ = "EI"
  halt _ = "HALT"
  stop _ = "STOP"
  invalid _ b = ".data " <> formatHex b
