{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module GBC.CPU.ISA
  ( RegisterR(..)
  , RegisterDD
  , RegisterSS(..)
  , RegisterQQ(..)
  , ConditionCode(..)
  , MonadGMBZ80(..)
  , DisassembleT(..)
  , runDisassembleT
  )
where

import           Common
import           Control.Monad.Reader
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

class MonadGMBZ80 m where
  type ExecuteResult m
  ldrr    :: RegisterR -> RegisterR -> m (ExecuteResult m)
  ldrn    :: RegisterR -> Word8 -> m (ExecuteResult m)
  ldrHL   :: RegisterR -> m (ExecuteResult m)
  ldHLr   :: RegisterR -> m (ExecuteResult m)
  ldHLn   :: Word8 -> m (ExecuteResult m)
  ldaBC   :: m (ExecuteResult m)
  ldaDE   :: m (ExecuteResult m)
  ldaC    :: m (ExecuteResult m)
  ldCa    :: m (ExecuteResult m)
  ldan    :: Word8 -> m (ExecuteResult m)
  ldna    :: Word8 -> m (ExecuteResult m)
  ldann   :: Word16 -> m (ExecuteResult m)
  ldnna   :: Word16 -> m (ExecuteResult m)
  ldaHLI  :: m (ExecuteResult m)
  ldaHLD  :: m (ExecuteResult m)
  ldBCa   :: m (ExecuteResult m)
  ldDEa   :: m (ExecuteResult m)
  ldHLIa  :: m (ExecuteResult m)
  ldHLDa  :: m (ExecuteResult m)
  ldddnn  :: RegisterDD -> Word16 -> m (ExecuteResult m)
  ldSPHL  :: m (ExecuteResult m)
  push    :: RegisterQQ -> m (ExecuteResult m)
  pop     :: RegisterQQ -> m (ExecuteResult m)
  ldhl    :: Int8 -> m (ExecuteResult m)
  ldnnSP  :: Word16 -> m (ExecuteResult m)
  addr    :: RegisterR -> m (ExecuteResult m)
  addn    :: Word8 -> m (ExecuteResult m)
  addhl   :: m (ExecuteResult m)
  adcr    :: RegisterR -> m (ExecuteResult m)
  adcn    :: Word8 -> m (ExecuteResult m)
  adchl   :: m (ExecuteResult m)
  subr    :: RegisterR -> m (ExecuteResult m)
  subn    :: Word8 -> m (ExecuteResult m)
  subhl   :: m (ExecuteResult m)
  sbcr    :: RegisterR -> m (ExecuteResult m)
  sbcn    :: Word8 -> m (ExecuteResult m)
  sbchl   :: m (ExecuteResult m)
  andr    :: RegisterR -> m (ExecuteResult m)
  andn    :: Word8 -> m (ExecuteResult m)
  andhl   :: m (ExecuteResult m)
  orr     :: RegisterR -> m (ExecuteResult m)
  orn     :: Word8 -> m (ExecuteResult m)
  orhl    :: m (ExecuteResult m)
  xorr    :: RegisterR -> m (ExecuteResult m)
  xorn    :: Word8 -> m (ExecuteResult m)
  xorhl   :: m (ExecuteResult m)
  cpr     :: RegisterR -> m (ExecuteResult m)
  cpn     :: Word8 -> m (ExecuteResult m)
  cphl    :: m (ExecuteResult m)
  incr    :: RegisterR -> m (ExecuteResult m)
  inchl   :: m (ExecuteResult m)
  decr    :: RegisterR -> m (ExecuteResult m)
  dechl   :: m (ExecuteResult m)
  addhlss :: RegisterSS -> m (ExecuteResult m)
  addSP   :: Int8 -> m (ExecuteResult m)
  incss   :: RegisterSS -> m (ExecuteResult m)
  decss   :: RegisterSS -> m (ExecuteResult m)
  rlca    :: m (ExecuteResult m)
  rla     :: m (ExecuteResult m)
  rrca    :: m (ExecuteResult m)
  rra     :: m (ExecuteResult m)
  rlcr    :: RegisterR -> m (ExecuteResult m)
  rlchl   :: m (ExecuteResult m)
  rlr     :: RegisterR -> m (ExecuteResult m)
  rlhl    :: m (ExecuteResult m)
  rrcr    :: RegisterR -> m (ExecuteResult m)
  rrchl   :: m (ExecuteResult m)
  rrr     :: RegisterR -> m (ExecuteResult m)
  rrhl    :: m (ExecuteResult m)
  slar    :: RegisterR -> m (ExecuteResult m)
  slahl   :: m (ExecuteResult m)
  srar    :: RegisterR -> m (ExecuteResult m)
  srahl   :: m (ExecuteResult m)
  srlr    :: RegisterR -> m (ExecuteResult m)
  srlhl   :: m (ExecuteResult m)
  swapr   :: RegisterR -> m (ExecuteResult m)
  swaphl  :: m (ExecuteResult m)
  bitr    :: RegisterR -> Word8 -> m (ExecuteResult m)
  bithl   :: Word8 -> m (ExecuteResult m)
  setr    :: RegisterR -> Word8 -> m (ExecuteResult m)
  sethl   :: Word8 -> m (ExecuteResult m)
  resr    :: RegisterR -> Word8 -> m (ExecuteResult m)
  reshl   :: Word8 -> m (ExecuteResult m)
  jpnn    :: Word16 -> m (ExecuteResult m)
  jphl    :: m (ExecuteResult m)
  jpccnn  :: ConditionCode -> Word16 -> m (ExecuteResult m)
  jr      :: Int8 -> m (ExecuteResult m)
  jrcc    :: ConditionCode -> Int8 -> m (ExecuteResult m)
  call    :: Word16 -> m (ExecuteResult m)
  callcc  :: ConditionCode -> Word16 -> m (ExecuteResult m)
  ret     :: m (ExecuteResult m)
  reti    :: m (ExecuteResult m)
  retcc   :: ConditionCode -> m (ExecuteResult m)
  rst     :: Word8 -> m (ExecuteResult m)
  daa     :: m (ExecuteResult m)
  cpl     :: m (ExecuteResult m)
  nop     :: m (ExecuteResult m)
  ccf     :: m (ExecuteResult m)
  scf     :: m (ExecuteResult m)
  di      :: m (ExecuteResult m)
  ei      :: m (ExecuteResult m)
  halt    :: m (ExecuteResult m)
  stop    :: m (ExecuteResult m)
  invalid :: Word8 -> m (ExecuteResult m)

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

newtype DisassembleT m a = DisassembleT (ReaderT (Word16 -> Maybe String) m a)
  deriving (Monad, Functor, Applicative, MonadTrans, MonadFix, MonadIO)

runDisassembleT :: DisassembleT m a -> (Word16 -> Maybe String) -> m a
runDisassembleT (DisassembleT r) = runReaderT r

lookupAddress :: Monad m => Word16 -> DisassembleT m String
lookupAddress address = DisassembleT $ do
  codeMap <- ask
  pure (maybe "" ('@' :) (codeMap address))

instance Monad m => MonadGMBZ80 (DisassembleT m) where
  type ExecuteResult (DisassembleT m) = String
  ldrr r r' = pure ("LD " <> format r <> ", " <> format r')
  ldrn r n = pure ("LD " <> format r <> ", " <> formatHex n)
  ldrHL r = pure ("LD " <> format r <> ", (HL)")
  ldHLr r = pure ("LD (HL), " <> format r)
  ldHLn n = pure ("LD (HL), " <> formatHex n)
  ldaBC = pure "LD A, (BC)"
  ldaDE = pure "LD A, (DE)"
  ldaC  = pure "LD A, (C)"
  ldCa  = pure "LD (C), A"
  ldan n = do
    address <- lookupAddress (0xFF00 + fromIntegral n)
    pure ("LD A, (FF" <> formatHex n <> address <> ")")
  ldna n = do
    address <- lookupAddress (0xFF00 + fromIntegral n)
    pure ("LD (FF" <> formatHex n <> address <> "), A")
  ldann nn = do
    address <- lookupAddress nn
    pure ("LD A, (" <> formatHex nn <> address <> ")")
  ldnna nn = do
    address <- lookupAddress nn
    pure ("LD (" <> formatHex nn <> address <> "), A")
  ldaHLI = pure "LD A, (HLI)"
  ldaHLD = pure "LD A, (HLD)"
  ldBCa  = pure "LD (BC), A"
  ldDEa  = pure "LD (DE), A"
  ldHLIa = pure "LD (HLI), A"
  ldHLDa = pure "LD (HLD), A"
  ldddnn dd nn = pure ("LD " <> format dd <> ", " <> formatHex nn)
  ldSPHL = pure "LD SP, HL"
  push qq = pure ("PUSH " <> format qq)
  pop qq = pure ("POP " <> format qq)
  ldhl i = pure ("LDHL SP, " <> formatHex i)
  ldnnSP nn = do
    address <- lookupAddress nn
    pure ("LD (" <> formatHex nn <> address <> "), SP")
  addr r = pure ("ADD A, " <> format r)
  addn w = pure ("ADD A, " <> formatHex w)
  addhl = pure "ADD A, (HL)"
  adcr r = pure ("ADC A, " <> format r)
  adcn i = pure ("ADC A, " <> formatHex i)
  adchl = pure "ADC A, (HL)"
  subr r = pure ("SUB A, " <> format r)
  subn i = pure ("SUB A, " <> formatHex i)
  subhl = pure "SUB A, (HL)"
  sbcr r = pure ("SBC A, " <> format r)
  sbcn i = pure ("SBC A, " <> formatHex i)
  sbchl = pure "SBC A, (HL)"
  andr r = pure ("AND A, " <> format r)
  andn i = pure ("AND A, " <> formatHex i)
  andhl = pure "AND A, (HL)"
  orr r = pure ("OR A, " <> format r)
  orn i = pure ("OR A, " <> formatHex i)
  orhl = pure "OR A, (HL)"
  xorr r = pure ("XOR A, " <> format r)
  xorn i = pure ("XOR A, " <> formatHex i)
  xorhl = pure "XOR A, (HL)"
  cpr r = pure ("CP A, " <> format r)
  cpn i = pure ("CP A, " <> formatHex i)
  cphl = pure "CP A, (HL)"
  incr r = pure ("INC " <> format r)
  inchl = pure "INC (HL)"
  decr r = pure ("DEC " <> format r)
  dechl = pure "DEC (HL)"
  addhlss ss = pure ("ADD HL, " <> format ss)
  addSP i = pure ("ADD SP, " <> formatHex i)
  incss ss = pure ("INC " <> format ss)
  decss ss = pure ("DEC " <> format ss)
  rlca = pure "RLCA"
  rla  = pure "RLA"
  rrca = pure "RRCA"
  rra  = pure "RRA"
  rlcr r = pure ("RLC " <> format r)
  rlchl = pure "RLC (HL)"
  rlr r = pure ("RL " <> format r)
  rlhl = pure "RL (HL)"
  rrcr r = pure ("RRC " <> format r)
  rrchl = pure "RRC (HL)"
  rrr r = pure ("RR " <> format r)
  rrhl = pure "RR (HL)"
  slar r = pure ("SLA " <> format r)
  slahl = pure "SLA (HL)"
  srar r = pure ("SRA " <> format r)
  srahl = pure "SRA (HL)"
  srlr r = pure ("SRL " <> format r)
  srlhl = pure "SRL (HL)"
  swapr r = pure ("SWAP " <> format r)
  swaphl = pure "SWAP (HL)"
  bitr r i = pure ("BIT " <> show i <> ", " <> format r)
  bithl i = pure ("BIT " <> show i <> ", (HL)")
  setr r i = pure ("SET " <> show i <> ", " <> format r)
  sethl i = pure ("SET " <> show i <> ", (HL)")
  resr r i = pure ("RES " <> show i <> ", " <> format r)
  reshl i = pure ("RES " <> show i <> ", (HL)")
  jpnn nn = do
    address <- lookupAddress nn
    pure ("JP " <> formatHex nn <> address)
  jphl = pure "JP (HL)"
  jpccnn cc nn = do
    address <- lookupAddress nn
    pure ("JP " <> format cc <> ", " <> formatHex nn <> address)
  jr i = pure ("JR " <> formatHex i)
  jrcc cc i = pure ("JR " <> format cc <> ", " <> formatHex i)
  call nn = do
    address <- lookupAddress nn
    pure ("CALL " <> formatHex nn <> address)
  callcc cc nn = do
    address <- lookupAddress nn
    pure ("CALL " <> format cc <> ", " <> formatHex nn <> address)
  ret  = pure "RET"
  reti = pure "RETI"
  retcc cc = pure ("RET " <> format cc)
  rst i = pure ("RST " <> show i)
  daa  = pure "DAA"
  cpl  = pure "CPL"
  nop  = pure "NOP"
  ccf  = pure "CCF"
  scf  = pure "SCF"
  di   = pure "DI"
  ei   = pure "EI"
  halt = pure "HALT"
  stop = pure "STOP"
  invalid b = pure (".data " <> formatHex b)
