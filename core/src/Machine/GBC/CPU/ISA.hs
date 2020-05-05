{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.CPU.ISA
  ( Register8(..)
  , RegisterHalf(..)
  , Register16(..)
  , RegisterPushPop(..)
  , ConditionCode(..)
  , MonadGMBZ80(..)
  )
where

import           Data.Int
import           Data.Word

-- | An 8-bit register
data Register8 = RegA | RegB | RegC | RegD | RegE | RegH | RegL deriving (Eq, Ord, Bounded, Enum)

-- | Half-words of 16-bit registers.
data RegisterHalf = RegSPH | RegSPL | RegPCH | RegPCL deriving (Eq, Ord, Bounded, Enum)

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
