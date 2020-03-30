{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.CPU.ISA
  ( Register8(..)
  , Register16(..)
  , RegisterPushPop(..)
  , ConditionCode(..)
  , MonadGMBZ80(..)
  , DisassembleT(..)
  , runDisassembleT
  )
where

import           Control.Monad.Reader
import           Data.Int
import           Data.Word
import           Machine.GBC.Util

-- | An 8-bit register
data Register8 = RegA | RegB | RegC | RegD | RegE | RegH | RegL deriving (Eq, Ord, Bounded, Enum)

-- | A 16-bit register.
data Register16 = RegSP | RegBC | RegDE | RegHL deriving (Eq, Ord, Bounded, Enum)

-- | A 16-bit register (for push and pop instructions).
data RegisterPushPop = PushPopAF | PushPopBC | PushPopDE | PushPopHL deriving (Eq, Ord, Bounded, Enum)

-- | A condition.
data ConditionCode = CondNZ | CondZ | CondNC | CondC deriving (Eq, Ord, Bounded, Enum)

class MonadGMBZ80 m where
  type ExecuteResult m
  ldrr    :: Register8 -> Register8 -> m (ExecuteResult m)
  ldrn    :: Register8 -> Word8 -> m (ExecuteResult m)
  ldrHL   :: Register8 -> m (ExecuteResult m)
  ldHLr   :: Register8 -> m (ExecuteResult m)
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
  ldddnn  :: Register16 -> Word16 -> m (ExecuteResult m)
  ldSPHL  :: m (ExecuteResult m)
  push    :: RegisterPushPop -> m (ExecuteResult m)
  pop     :: RegisterPushPop -> m (ExecuteResult m)
  ldhl    :: Int8 -> m (ExecuteResult m)
  ldnnSP  :: Word16 -> m (ExecuteResult m)
  addr    :: Register8 -> m (ExecuteResult m)
  addn    :: Word8 -> m (ExecuteResult m)
  addhl   :: m (ExecuteResult m)
  adcr    :: Register8 -> m (ExecuteResult m)
  adcn    :: Word8 -> m (ExecuteResult m)
  adchl   :: m (ExecuteResult m)
  subr    :: Register8 -> m (ExecuteResult m)
  subn    :: Word8 -> m (ExecuteResult m)
  subhl   :: m (ExecuteResult m)
  sbcr    :: Register8 -> m (ExecuteResult m)
  sbcn    :: Word8 -> m (ExecuteResult m)
  sbchl   :: m (ExecuteResult m)
  andr    :: Register8 -> m (ExecuteResult m)
  andn    :: Word8 -> m (ExecuteResult m)
  andhl   :: m (ExecuteResult m)
  orr     :: Register8 -> m (ExecuteResult m)
  orn     :: Word8 -> m (ExecuteResult m)
  orhl    :: m (ExecuteResult m)
  xorr    :: Register8 -> m (ExecuteResult m)
  xorn    :: Word8 -> m (ExecuteResult m)
  xorhl   :: m (ExecuteResult m)
  cpr     :: Register8 -> m (ExecuteResult m)
  cpn     :: Word8 -> m (ExecuteResult m)
  cphl    :: m (ExecuteResult m)
  incr    :: Register8 -> m (ExecuteResult m)
  inchl   :: m (ExecuteResult m)
  decr    :: Register8 -> m (ExecuteResult m)
  dechl   :: m (ExecuteResult m)
  addhlss :: Register16 -> m (ExecuteResult m)
  addSP   :: Int8 -> m (ExecuteResult m)
  incss   :: Register16 -> m (ExecuteResult m)
  decss   :: Register16 -> m (ExecuteResult m)
  rlca    :: m (ExecuteResult m)
  rla     :: m (ExecuteResult m)
  rrca    :: m (ExecuteResult m)
  rra     :: m (ExecuteResult m)
  rlcr    :: Register8 -> m (ExecuteResult m)
  rlchl   :: m (ExecuteResult m)
  rlr     :: Register8 -> m (ExecuteResult m)
  rlhl    :: m (ExecuteResult m)
  rrcr    :: Register8 -> m (ExecuteResult m)
  rrchl   :: m (ExecuteResult m)
  rrr     :: Register8 -> m (ExecuteResult m)
  rrhl    :: m (ExecuteResult m)
  slar    :: Register8 -> m (ExecuteResult m)
  slahl   :: m (ExecuteResult m)
  srar    :: Register8 -> m (ExecuteResult m)
  srahl   :: m (ExecuteResult m)
  srlr    :: Register8 -> m (ExecuteResult m)
  srlhl   :: m (ExecuteResult m)
  swapr   :: Register8 -> m (ExecuteResult m)
  swaphl  :: m (ExecuteResult m)
  bitr    :: Register8 -> Word8 -> m (ExecuteResult m)
  bithl   :: Word8 -> m (ExecuteResult m)
  setr    :: Register8 -> Word8 -> m (ExecuteResult m)
  sethl   :: Word8 -> m (ExecuteResult m)
  resr    :: Register8 -> Word8 -> m (ExecuteResult m)
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

instance Show Register8 where
  show RegA = "A"
  show RegB = "B"
  show RegC = "C"
  show RegD = "D"
  show RegE = "E"
  show RegH = "H"
  show RegL = "L"

instance Show Register16 where
  show RegSP = "SP"
  show RegBC = "BC"
  show RegDE = "DE"
  show RegHL = "HL"

instance Show RegisterPushPop where
  show PushPopAF = "AF"
  show PushPopBC = "BC"
  show PushPopDE = "DE"
  show PushPopHL = "HL"

instance Show ConditionCode where
  show CondNZ = "NZ"
  show CondZ  = "Z"
  show CondNC = "NC"
  show CondC  = "C"

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
  ldrr r r' = pure ("LD " <> show r <> ", " <> show r')
  ldrn r n = pure ("LD " <> show r <> ", " <> formatHex n)
  ldrHL r = pure ("LD " <> show r <> ", (HL)")
  ldHLr r = pure ("LD (HL), " <> show r)
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
  ldddnn dd nn = pure ("LD " <> show dd <> ", " <> formatHex nn)
  ldSPHL = pure "LD SP, HL"
  push qq = pure ("PUSH " <> show qq)
  pop qq = pure ("POP " <> show qq)
  ldhl i = pure ("LDHL SP, " <> formatHex i)
  ldnnSP nn = do
    address <- lookupAddress nn
    pure ("LD (" <> formatHex nn <> address <> "), SP")
  addr r = pure ("ADD A, " <> show r)
  addn w = pure ("ADD A, " <> formatHex w)
  addhl = pure "ADD A, (HL)"
  adcr r = pure ("ADC A, " <> show r)
  adcn i = pure ("ADC A, " <> formatHex i)
  adchl = pure "ADC A, (HL)"
  subr r = pure ("SUB A, " <> show r)
  subn i = pure ("SUB A, " <> formatHex i)
  subhl = pure "SUB A, (HL)"
  sbcr r = pure ("SBC A, " <> show r)
  sbcn i = pure ("SBC A, " <> formatHex i)
  sbchl = pure "SBC A, (HL)"
  andr r = pure ("AND A, " <> show r)
  andn i = pure ("AND A, " <> formatHex i)
  andhl = pure "AND A, (HL)"
  orr r = pure ("OR A, " <> show r)
  orn i = pure ("OR A, " <> formatHex i)
  orhl = pure "OR A, (HL)"
  xorr r = pure ("XOR A, " <> show r)
  xorn i = pure ("XOR A, " <> formatHex i)
  xorhl = pure "XOR A, (HL)"
  cpr r = pure ("CP A, " <> show r)
  cpn i = pure ("CP A, " <> formatHex i)
  cphl = pure "CP A, (HL)"
  incr r = pure ("INC " <> show r)
  inchl = pure "INC (HL)"
  decr r = pure ("DEC " <> show r)
  dechl = pure "DEC (HL)"
  addhlss ss = pure ("ADD HL, " <> show ss)
  addSP i = pure ("ADD SP, " <> formatHex i)
  incss ss = pure ("INC " <> show ss)
  decss ss = pure ("DEC " <> show ss)
  rlca = pure "RLCA"
  rla  = pure "RLA"
  rrca = pure "RRCA"
  rra  = pure "RRA"
  rlcr r = pure ("RLC " <> show r)
  rlchl = pure "RLC (HL)"
  rlr r = pure ("RL " <> show r)
  rlhl = pure "RL (HL)"
  rrcr r = pure ("RRC " <> show r)
  rrchl = pure "RRC (HL)"
  rrr r = pure ("RR " <> show r)
  rrhl = pure "RR (HL)"
  slar r = pure ("SLA " <> show r)
  slahl = pure "SLA (HL)"
  srar r = pure ("SRA " <> show r)
  srahl = pure "SRA (HL)"
  srlr r = pure ("SRL " <> show r)
  srlhl = pure "SRL (HL)"
  swapr r = pure ("SWAP " <> show r)
  swaphl = pure "SWAP (HL)"
  bitr r i = pure ("BIT " <> show i <> ", " <> show r)
  bithl i = pure ("BIT " <> show i <> ", (HL)")
  setr r i = pure ("SET " <> show i <> ", " <> show r)
  sethl i = pure ("SET " <> show i <> ", (HL)")
  resr r i = pure ("RES " <> show i <> ", " <> show r)
  reshl i = pure ("RES " <> show i <> ", (HL)")
  jpnn nn = do
    address <- lookupAddress nn
    pure ("JP " <> formatHex nn <> address)
  jphl = pure "JP (HL)"
  jpccnn cc nn = do
    address <- lookupAddress nn
    pure ("JP " <> show cc <> ", " <> formatHex nn <> address)
  jr i = pure ("JR " <> formatHex i)
  jrcc cc i = pure ("JR " <> show cc <> ", " <> formatHex i)
  call nn = do
    address <- lookupAddress nn
    pure ("CALL " <> formatHex nn <> address)
  callcc cc nn = do
    address <- lookupAddress nn
    pure ("CALL " <> show cc <> ", " <> formatHex nn <> address)
  ret  = pure "RET"
  reti = pure "RETI"
  retcc cc = pure ("RET " <> show cc)
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