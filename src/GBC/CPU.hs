{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module GBC.CPU
  ( RegisterFile(..)
  , CPUMode(..)
  , CPUState(..)
  , HasCPUState(..)
  , UsesCPU
  , BusEvent(..)
  , initCPU
  , getMode
  , reset
  , decodeOnly
  , getRegisterFile
  , readR8
  , writeR8
  , readR16
  , writeR16
  , readPC
  , writePC
  , readF
  , writeF
  , testFlag
  , setFlags
  , setFlagsMask
  , setIME
  , clearIME
  , testIME
  , flagCY
  , flagN
  , flagH
  , flagZ
  , flagIME
  , testCondition
  , readOperand8
  , readSmallOperand8
  , writeSmallOperand8
  , raiseInterrupt
  , executeInstruction
  , cpuStep
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Int
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GBC.Decode
import           GBC.ISA
import           Common
import           GBC.Memory

-- | The register file.
data RegisterFile = RegisterFile {
    regA :: !Word8
  , regB :: !Word8
  , regC :: !Word8
  , regD :: !Word8
  , regE :: !Word8
  , regF :: !Word8
  , regH :: !Word8
  , regL :: !Word8
  , regSP :: !Word16
  , regPC :: !Word16
  , regHidden :: !Word16
} deriving (Eq, Ord, Show)

offsetF, offsetA, offsetC, offsetB :: Int
offsetE, offsetD, offsetL, offsetH, offsetPC, offsetSP, offsetHidden :: Int
offsetF = 0
offsetA = 1
offsetC = 2
offsetB = 3
offsetE = 4
offsetD = 5
offsetL = 6
offsetH = 7
offsetPC = 8
offsetSP = 10
offsetHidden = 12

instance Storable RegisterFile where
  sizeOf _ = 14
  alignment _ = 2
  peek ptr = do
    regA      <- peekByteOff ptr offsetA
    regB      <- peekByteOff ptr offsetB
    regC      <- peekByteOff ptr offsetC
    regD      <- peekByteOff ptr offsetD
    regE      <- peekByteOff ptr offsetE
    regF      <- peekByteOff ptr offsetF
    regH      <- peekByteOff ptr offsetH
    regL      <- peekByteOff ptr offsetL
    regSP     <- peekByteOff ptr offsetSP
    regPC     <- peekByteOff ptr offsetPC
    regHidden <- peekByteOff ptr offsetHidden
    pure RegisterFile { .. }
  poke ptr RegisterFile {..} = do
    pokeByteOff ptr offsetA      regA
    pokeByteOff ptr offsetB      regB
    pokeByteOff ptr offsetC      regC
    pokeByteOff ptr offsetD      regD
    pokeByteOff ptr offsetE      regE
    pokeByteOff ptr offsetF      regF
    pokeByteOff ptr offsetH      regH
    pokeByteOff ptr offsetL      regL
    pokeByteOff ptr offsetSP     regSP
    pokeByteOff ptr offsetPC     regPC
    pokeByteOff ptr offsetHidden regHidden

-- | Information for the debugger.
data BusEvent = BusEvent {
   writeAddress :: ![Word16]
 , clockAdvance :: !Int
} deriving (Eq, Ord, Show)

-- | The current CPU mode.
data CPUMode = ModeHalt | ModeStop | ModeNormal deriving (Eq, Ord, Show, Bounded, Enum)

-- | The internal CPU state.
data CPUState = CPUState {
    registers :: !(ForeignPtr RegisterFile)
  , cpuMode :: !(IORef CPUMode)
}

class HasCPUState env where
  forCPUState :: env -> CPUState

-- | Initialize a new CPU.
initCPU :: IO CPUState
initCPU = CPUState <$> mallocForeignPtr <*> newIORef ModeNormal

-- | Constraints for monads that update the CPU.
type UsesCPU env m = (UsesMemory env m, HasCPUState env)

-- | Get the current cpu mode.
{-# INLINABLE getMode #-}
getMode :: UsesCPU env m => ReaderT env m CPUMode
getMode = do
  CPUState {..} <- asks forCPUState
  liftIO $ readIORef cpuMode

-- | Get the CPU mode.
{-# INLINABLE setMode #-}
setMode :: UsesCPU env m => CPUMode -> ReaderT env m ()
setMode mode = do
  CPUState {..} <- asks forCPUState
  liftIO $ writeIORef cpuMode mode

-- | Get the values of all the registers.
{-# INLINABLE getRegisterFile #-}
getRegisterFile :: UsesCPU env m => ReaderT env m RegisterFile
getRegisterFile = do
  CPUState {..} <- asks forCPUState
  liftIO $ withForeignPtr registers peek

-- | Read data from the register file.
{-# INLINE readRegister #-}
readRegister :: (UsesCPU env m, Storable a) => Int -> ReaderT env m a
readRegister offset = do
  CPUState {..} <- asks forCPUState
  liftIO $ withForeignPtr registers $ flip peekByteOff offset

-- | Write data to the register file.
{-# INLINE writeRegister #-}
writeRegister :: (UsesCPU env m, Storable a) => Int -> a -> ReaderT env m ()
writeRegister offset value = do
  CPUState {..} <- asks forCPUState
  liftIO $ withForeignPtr registers $ \ptr -> pokeByteOff ptr offset value

-- | Read a single register.
{-# INLINABLE readR8 #-}
readR8 :: UsesCPU env m => Register8 -> ReaderT env m Word8
readR8 = readRegister . offsetR8

-- | Write a single register.
{-# INLINABLE writeR8 #-}
writeR8 :: UsesCPU env m => Register8 -> Word8 -> ReaderT env m ()
writeR8 register = writeRegister $ offsetR8 register

-- | Get the offset in the register file of a single register.
offsetR8 :: Register8 -> Int
offsetR8 RegA = offsetA
offsetR8 RegB = offsetB
offsetR8 RegC = offsetC
offsetR8 RegD = offsetD
offsetR8 RegE = offsetE
offsetR8 RegH = offsetH
offsetR8 RegL = offsetL

-- | Read a 16-bit register.
{-# INLINABLE readR16 #-}
readR16 :: UsesCPU env m => Register16 -> ReaderT env m Word16
readR16 = readRegister . offsetR16

-- | Write a 16-bit register.
{-# INLINABLE writeR16 #-}
writeR16 :: UsesCPU env m => Register16 -> Word16 -> ReaderT env m ()
writeR16 register = writeRegister $ offsetR16 register

-- | Get the offset in the register file of a register pair.
offsetR16 :: Register16 -> Int
offsetR16 RegBC = offsetC
offsetR16 RegDE = offsetE
offsetR16 RegHL = offsetL
offsetR16 RegSP = offsetSP

-- | Read the PC register.
{-# INLINABLE readPC #-}
readPC :: UsesCPU env m => ReaderT env m Word16
readPC = readRegister offsetPC

-- | Write the PC register.
{-# INLINABLE writePC #-}
writePC :: UsesCPU env m => Word16 -> ReaderT env m ()
writePC = writeRegister offsetPC

-- | Read the AF register.
{-# INLINABLE readAF #-}
readAF :: UsesCPU env m => ReaderT env m Word16
readAF = readRegister offsetF

-- | Write the AF register.
{-# INLINABLE writeAF #-}
writeAF :: UsesCPU env m => Word16 -> ReaderT env m ()
writeAF = writeRegister offsetF

-- | Read from a 'Operand8'. Returns a list of addresses that were read from.
{-# INLINE readOperand8 #-}
readOperand8 :: UsesCPU env m => Operand8 -> ReaderT env m Word8
readOperand8 (R8 register) = readR8 register
readOperand8 (I8 value   ) = pure value
readOperand8 HLI           = readByte =<< readR16 RegHL

-- | Read from a 'SmallOperand8'.  Returns a list of addresses that were read from.
{-# INLINABLE readSmallOperand8 #-}
readSmallOperand8 :: UsesCPU env m => SmallOperand8 -> ReaderT env m Word8
readSmallOperand8 (SmallR8 register) = readR8 register
readSmallOperand8 SmallHLI           = readOperand8 HLI

-- | Write to a 'SmallOperand8'.  Returns a list of addresses that were written to.
{-# INLINABLE writeSmallOperand8 #-}
writeSmallOperand8 :: UsesCPU env m => SmallOperand8 -> Word8 -> ReaderT env m [Word16]
writeSmallOperand8 (SmallR8 register) value = [] <$ writeR8 register value
writeSmallOperand8 SmallHLI           value = do
  hl <- readR16 RegHL
  writeMem hl value
  pure [hl]

type Flag = Word8
flagZ, flagN, flagH, flagCY :: Flag
flagZ = 0x10
flagN = 0x20
flagH = 0x40
flagCY = 0x80

-- Master interrupt enable flag
{-# INLINABLE flagIME #-}
flagIME :: Word16
flagIME = 0x0100

-- | Check if a flag is set.
{-# INLINABLE testFlag #-}
testFlag :: UsesCPU env m => Flag -> ReaderT env m Bool
testFlag flag = do
  f <- readRegister offsetF
  pure $ f .&. flag /= 0

-- | Check if a condition code is true.
{-# INLINABLE testCondition #-}
testCondition :: UsesCPU env m => ConditionCode -> ReaderT env m Bool
testCondition CondNZ = not <$> testFlag flagZ
testCondition CondZ  = testFlag flagZ
testCondition CondNC = not <$> testFlag flagCY
testCondition CondC  = testFlag flagCY

-- | Read the F register.
{-# INLINABLE readF #-}
readF :: UsesCPU env m => ReaderT env m Word8
readF = readRegister offsetF

-- | Write the F register.
{-# INLINABLE writeF #-}
writeF :: UsesCPU env m => Word8 -> ReaderT env m ()
writeF = writeRegister offsetF

-- | Set all the flags.
{-# INLINABLE setFlags #-}
setFlags :: UsesCPU env m => Word8 -> ReaderT env m ()
setFlags = setFlagsMask 0xF0

-- | Set some flags.
{-# INLINABLE setFlagsMask #-}
setFlagsMask
  :: UsesCPU env m
  => Word8 -- ^ bitmask containing flags to set.
  -> Word8 -- ^ new flags values.
  -> ReaderT env m ()
setFlagsMask mask flags = do
  oldFlags <- readRegister offsetF
  writeRegister offsetF $ (oldFlags .&. complement mask) .|. (flags .&. mask)

-- | Set the master interrupt flag.
{-# INLINABLE setIME #-}
setIME :: UsesCPU env m => ReaderT env m ()
setIME = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .|. flagIME)

-- | Clear the master interrupt flag.
{-# INLINABLE clearIME #-}
clearIME :: UsesCPU env m => ReaderT env m ()
clearIME = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .&. complement flagIME)

-- | Check the status of the interrupt flag.
{-# INLINABLE testIME #-}
testIME :: UsesCPU env m => ReaderT env m Bool
testIME = do
  ime <- readRegister offsetHidden
  pure $ ime .&. flagIME /= 0

-- | Reset the CPU.
{-# INLINABLE reset #-}
reset :: UsesCPU env m => ReaderT env m ()
reset = do
  writeR8 RegA 0x01
  writeF 0xB0
  writeR8 RegB 0x00
  writeR8 RegC 0x13
  writeR8 RegD 0x00
  writeR8 RegE 0xD8
  writeR8 RegH 0x01
  writeR8 RegL 0x4D
  writeR16 RegSP 0xFFFE
  writeRegister offsetHidden (0 :: Word8)
  setIME
  writePC 0x150

  writeMem 0xFF05 (0x00 :: Word8)   -- TIMA
  writeMem 0xFF06 (0x00 :: Word8)   -- TMA
  writeMem 0xFF07 (0x00 :: Word8)   -- TAC
  writeMem 0xFF10 (0x80 :: Word8)   -- NR10
  writeMem 0xFF11 (0xBF :: Word8)   -- NR11
  writeMem 0xFF12 (0xF3 :: Word8)   -- NR12
  writeMem 0xFF14 (0xBF :: Word8)   -- NR14
  writeMem 0xFF16 (0x3F :: Word8)   -- NR21
  writeMem 0xFF17 (0x00 :: Word8)   -- NR22
  writeMem 0xFF19 (0xBF :: Word8)   -- NR24
  writeMem 0xFF1A (0x7F :: Word8)   -- NR30
  writeMem 0xFF1B (0xFF :: Word8)   -- NR31
  writeMem 0xFF1C (0x9F :: Word8)   -- NR32
  writeMem 0xFF1E (0xBF :: Word8)   -- NR33
  writeMem 0xFF20 (0xFF :: Word8)   -- NR41
  writeMem 0xFF21 (0x00 :: Word8)   -- NR42
  writeMem 0xFF22 (0x00 :: Word8)   -- NR43
  writeMem 0xFF23 (0xBF :: Word8)   -- NR30
  writeMem 0xFF24 (0x77 :: Word8)   -- NR50
  writeMem 0xFF25 (0xF3 :: Word8)   -- NR51
  writeMem 0xFF26 (0xF1 :: Word8)   -- NR52
  writeMem 0xFF40 (0x91 :: Word8)   -- LCDC
  writeMem 0xFF42 (0x00 :: Word8)   -- SCY
  writeMem 0xFF43 (0x00 :: Word8)   -- SCX
  writeMem 0xFF45 (0x00 :: Word8)   -- LYC
  writeMem 0xFF47 (0xFC :: Word8)   -- BGP
  writeMem 0xFF48 (0xFF :: Word8)   -- OBP0
  writeMem 0xFF49 (0xFF :: Word8)   -- OBP1
  writeMem 0xFF4A (0x00 :: Word8)   -- WY
  writeMem 0xFF4B (0x00 :: Word8)   -- WX
  writeMem IE (0x00 :: Word8)       -- IE
  writeMem IF (0x00 :: Word8)       -- IF

-- | An arithmetic operation.
data ArithmeticOp = OpAdd | OpSub deriving (Eq, Ord, Show, Bounded, Enum)

-- | Perform an arithmetic operation and adjust the flags.
{-# INLINE adder8 #-}
adder8 :: ArithmeticOp -> Word8 -> Word8 -> Bool -> (Word8, Word8)
adder8 op a1 a2 carry =
  let (wa1, wa2) = (fromIntegral a1 :: Word16, fromIntegral a2)
      wr         = case op of
        OpAdd -> wa1 + wa2 + (if carry then 1 else 0)
        OpSub -> wa1 - wa2 - (if carry then 1 else 0)
      r       = fromIntegral wr
      carryH  = (wa1 .&. 0x0010) `xor` (wa2 .&. 0x0010) /= (wr .&. 0x0010)
      carryCY = (wa1 .&. 0x0100) `xor` (wa2 .&. 0x0100) /= (wr .&. 0x0100)
      flags =
          (if r == 0 then flagZ else 0)
            .|. (if op == OpSub then flagN else 0)
            .|. (if carryH then flagH else 0)
            .|. (if carryCY then flagCY else 0)
  in  (r, flags)

-- | Perform an increment operation and adjust the flags.
{-# INLINE inc8 #-}
inc8 :: ArithmeticOp -> Word8 -> (Word8, Word8)
inc8 op value =
  let r = case op of
        OpAdd -> value + 1
        OpSub -> value - 1
      carryH = (value .&. 0x10) /= (r .&. 0x010)
      flags =
          (if r == 0 then flagZ else 0)
            .|. (if op == OpSub then flagN else 0)
            .|. (if carryH then flagH else 0)
  in  (r, flags)

-- | Decode an instruction and advance the PC.
{-# INLINABLE decodeAndAdvancePC #-}
decodeAndAdvancePC :: UsesCPU env m => Decode a -> ReaderT env m a
decodeAndAdvancePC action = do
  pc       <- readPC
  (r, pc') <- runDecode pc action
  writePC pc'
  pure r

-- | Decode an instruction.
{-# INLINABLE decodeOnly #-}
decodeOnly :: UsesCPU env m => Decode a -> ReaderT env m a
decodeOnly action = do
  pc     <- readPC
  (r, _) <- runDecode pc action
  pure r

-- | IE register
pattern IE :: Word16
pattern IE = 0xFFFF

-- | IF register
pattern IF :: Word16
pattern IF = 0xFF0F

interruptVector :: Int -> Word16
interruptVector 0 = 0x40
interruptVector 1 = 0x48
interruptVector 2 = 0x50
interruptVector 3 = 0x58
interruptVector 4 = 0x60
interruptVector n = error $ "Invalid interrupt vector " ++ show n

-- | Get all of the pending interrupts that are ready to service.
{-# INLINE pendingEnabledInterrupts #-}
pendingEnabledInterrupts :: UsesCPU env m => ReaderT env m Word8
pendingEnabledInterrupts = do
  interrupt <- readByte IF
  enabled   <- readByte IE
  pure $ interrupt .&. enabled .&. 0x1F

-- | Get the next interrupt to service.
{-# INLINE getNextInterrupt #-}
getNextInterrupt :: Word8 -> Int
getNextInterrupt = countTrailingZeros

-- | Raise an interrupt.
raiseInterrupt ::  UsesMemory env m => Int -> ReaderT env m ()
raiseInterrupt interrupt = writeMem IF =<< (`setBit` interrupt) <$> readByte IF

-- | Fetch, decode, and execute a single instruction.
{-# INLINABLE cpuStep #-}
cpuStep :: UsesCPU env m => ReaderT env m BusEvent
cpuStep = do
  -- Check if we have an interrupt
  interrupts <- pendingEnabledInterrupts
  ime <- testIME

  if interrupts == 0 || not ime
    then executeInstruction =<< decodeAndAdvancePC decode
    else do
      -- Handle an interrupt
      let nextInterrupt = getNextInterrupt interrupts
      pc <- readPC
      sp <- readR16 RegSP
      let sp' = sp - 2
      writeMem sp' pc
      writeR16 RegSP sp'
      writePC $ interruptVector nextInterrupt
      clearIME
      writeMem IF $ clearBit interrupts nextInterrupt
      pure $ BusEvent [sp', sp' + 1] 28 -- TODO: Number of clocks here is just a guess

-- | Execute a single instruction.
{-# INLINABLE executeInstruction #-}
executeInstruction :: UsesCPU env m => Instruction -> ReaderT env m BusEvent
executeInstruction instruction = case instruction of
  -- LD r8 \<r8|im8|(HL)\>
  LD_R8 r8 o8 -> do
    value <- readOperand8 o8
    writeR8 r8 value
    pure $ BusEvent [] $ clocks instruction True
  -- LD (HL) r8
  LDHLI_R8 r8 -> do
    value <- readR8 r8
    hl    <- readR16 RegHL
    writeMem hl value
    pure $ BusEvent [hl] $ clocks instruction True
  -- LD (HL) im8
  LDHLI_I8 im8 -> do
    hl <- readR16 RegHL
    writeMem hl im8
    pure $ BusEvent [hl] $ clocks instruction True
  -- LD A (BC)
  LDA_BCI -> do
    bc    <- readR16 RegBC
    value <- readByte bc
    writeR8 RegA value
    pure $ BusEvent [] $ clocks instruction True
  -- LD A (DE)
  LDA_DEI -> do
    de    <- readR16 RegDE
    value <- readByte de
    writeR8 RegA value
    pure $ BusEvent [] $ clocks instruction True
  -- LD A (C)
  LDA_CI -> do
    c <- readR8 RegC
    let addr = fromIntegral c + 0xFF00
    value <- readByte addr
    writeR8 RegA value
    pure $ BusEvent [] $ clocks instruction True
  -- LD (C) A
  LDCI_A -> do
    c <- readR8 RegC
    let addr = fromIntegral c + 0xFF00
    value <- readR8 RegA
    writeMem addr value
    pure $ BusEvent [addr] $ clocks instruction True
  -- LD A (im8)
  LDA_I8I w8 -> do
    let addr = fromIntegral w8 + 0xFF00
    value <- readByte addr
    writeR8 RegA value
    pure $ BusEvent [] $ clocks instruction True
  -- LD (im8) A
  LDI8I_A w8 -> do
    let addr = fromIntegral w8 + 0xFF00
    value <- readR8 RegA
    writeMem addr value
    pure $ BusEvent [addr] $ clocks instruction True
  -- LD A (im16)
  LDA_I16I w16 -> do
    value <- readByte w16
    writeR8 RegA value
    pure $ BusEvent [] $ clocks instruction True
  -- LD (im16) A
  LDI16I_A w16 -> do
    value <- readR8 RegA
    writeMem w16 value
    pure $ BusEvent [w16] $ clocks instruction True
  -- LD A (HL++)
  LDA_INC -> do
    hl    <- readR16 RegHL
    value <- readByte hl
    writeR8 RegA value
    writeR16 RegHL (hl + 1)
    pure $ BusEvent [] $ clocks instruction True
  -- LD A (HL--)
  LDA_DEC -> do
    hl    <- readR16 RegHL
    value <- readByte hl
    writeR8 RegA value
    writeR16 RegHL (hl - 1)
    pure $ BusEvent [] $ clocks instruction True
  -- LD (BC) A
  LDBCI_A -> do
    value <- readR8 RegA
    addr  <- readR16 RegBC
    writeMem addr value
    pure $ BusEvent [addr] $ clocks instruction True
  -- LD (DE) A
  LDDEI_A -> do
    value <- readR8 RegA
    addr  <- readR16 RegDE
    writeMem addr value
    pure $ BusEvent [addr] $ clocks instruction True
  -- LD (HL++) A
  LDHLI_INC -> do
    value <- readR8 RegA
    hl    <- readR16 RegHL
    writeMem hl value
    writeR16 RegHL (hl + 1)
    pure $ BusEvent [hl] $ clocks instruction True
  -- LD (HL--) A
  LDHLI_DEC -> do
    value <- readR8 RegA
    hl    <- readR16 RegHL
    writeMem hl value
    writeR16 RegHL (hl - 1)
    pure $ BusEvent [hl] $ clocks instruction True
  -- LD r16 im16
  LD16_I16 r16 w16 -> do
    writeR16 r16 w16
    pure $ BusEvent [] $ clocks instruction True
  -- LD SP HL
  LDSP -> do
    writeR16 RegSP =<< readR16 RegHL
    pure $ BusEvent [] $ clocks instruction True
  -- PUSH r16
  PUSH r16 -> do
    sp <- readR16 RegSP
    let sp' = sp - 2
    value <- if r16 == RegSP then readAF else readR16 r16
    writeMem sp' value
    writeR16 RegSP sp'
    pure $ BusEvent [sp', sp' + 1] $ clocks instruction True
  -- POP r16
  POP r16 -> do
    sp     <- readR16 RegSP
    valueL <- fromIntegral <$> readByte sp
    valueH <- fromIntegral <$> readByte (sp + 1)
    let value = (valueH `unsafeShiftL` 8) .|. valueL
    if r16 == RegSP then writeAF value else writeR16 r16 value
    writeR16 RegSP (sp + 2)
    pure $ BusEvent [] $ clocks instruction True
  -- LDHL SP im8
  LDHL i8 -> do
    sp <- fromIntegral <$> readR16 RegSP
    let wi8     = fromIntegral i8 :: Word32
    let wr      = sp + wi8
    let carryH = (sp .&. 0x00001000) `xor` (wi8 .&. 0x00001000) /= (wr .&. 0x00001000)
    let carryCY = (sp .&. 0x00010000) `xor` (wi8 .&. 0x00010000) /= (wr .&. 0x00010000)
    writeR16 RegHL $ fromIntegral wr
    setFlags $ (if carryCY then flagCY else 0) .|. (if carryH then flagH else 0)
    pure $ BusEvent [] $ clocks instruction True
  -- LD (im16) SP
  LDI16I_SP w16 -> do
    sp <- readR16 RegSP
    writeMem w16 sp
    pure $ BusEvent [w16, w16 + 1] $ clocks instruction True
  -- ADD \<r8|im8|(HL)\>
  ADD o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let (r, flags) = adder8 OpAdd a arg False
    writeR8 RegA r
    setFlags flags
    pure $ BusEvent [] $ clocks instruction True
  -- ADC \<r8|im8|(HL)\>
  ADC o8 -> do
    a     <- readR8 RegA
    arg   <- readOperand8 o8
    carry <- testFlag flagCY
    let (r, flags) = adder8 OpAdd a arg carry
    writeR8 RegA r
    setFlags flags
    pure $ BusEvent [] $ clocks instruction True
  -- SUB \<r8|im8|(HL)\>
  SUB o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let (r, flags) = adder8 OpSub a arg False
    writeR8 RegA r
    setFlags flags
    pure $ BusEvent [] $ clocks instruction True
  -- SBC \<r8|im8|(HL)\>
  SBC o8 -> do
    a     <- readR8 RegA
    arg   <- readOperand8 o8
    carry <- testFlag flagCY
    let (r, flags) = adder8 OpSub a arg carry
    writeR8 RegA r
    setFlags flags
    pure $ BusEvent [] $ clocks instruction True
  -- AND \<r8|im8|(HL)\>
  AND o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let r = a .&. arg
    writeR8 RegA r
    setFlags $ flagH .|. (if r == 0 then flagZ else 0)
    pure $ BusEvent [] $ clocks instruction True
  -- OR \<r8|im8|(HL)\>
  OR o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let r = a .|. arg
    writeR8 RegA r
    setFlags $ if r == 0 then flagZ else 0
    pure $ BusEvent [] $ clocks instruction True
  -- XOR \<r8|im8|(HL)\>
  XOR o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let r = a `xor` arg
    writeR8 RegA r
    setFlags $ if r == 0 then flagZ else 0
    pure $ BusEvent [] $ clocks instruction True
  -- CP \<r8|im8|(HL)\>
  CP o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let (_, flags) = adder8 OpSub a arg False
    setFlags flags
    pure $ BusEvent [] $ clocks instruction True
  -- INC \<r8|(HL)\>
  INC so8 -> do
    value <- readSmallOperand8 so8
    let (r, flags) = inc8 OpAdd value
    wroteAddr <- writeSmallOperand8 so8 r
    setFlags flags
    pure $ BusEvent wroteAddr $ clocks instruction True
  -- DEC \<r8|(HL)\>
  DEC so8 -> do
    value <- readSmallOperand8 so8
    let (r, flags) = inc8 OpSub value
    wroteAddr <- writeSmallOperand8 so8 r
    setFlags flags
    pure $ BusEvent wroteAddr $ clocks instruction True
  -- ADD HL r16
  ADDHL r16 -> do
    hl <- fromIntegral <$> readR16 RegHL
    ss <- fromIntegral <$> readR16 r16
    let wr      = hl + ss :: Word32
    let carryH = (hl .&. 0x00001000) `xor` (ss .&. 0x00001000) /= (wr .&. 0x00001000)
    let carryCY = (hl .&. 0x00010000) `xor` (ss .&. 0x00010000) /= (wr .&. 0x00010000)
    writeR16 RegHL $ fromIntegral wr
    setFlagsMask (flagCY .|. flagH .|. flagN)
      $   (if carryH then flagH else 0)
      .|. (if carryCY then flagCY else 0)
    pure $ BusEvent [] $ clocks instruction True
  -- ADD SP im8
  ADDSP i8 -> do
    sp <- fromIntegral <$> readR16 RegSP
    let wi8     = fromIntegral i8
    let wr      = wi8 + sp :: Word32
    let carryH = (sp .&. 0x00001000) `xor` (wi8 .&. 0x00001000) /= (wr .&. 0x00001000)
    let carryCY = (sp .&. 0x00010000) `xor` (wi8 .&. 0x00010000) /= (wr .&. 0x00010000)
    writeR16 RegSP $ fromIntegral wr
    setFlags $ (if carryH then flagH else 0) .|. (if carryCY then flagCY else 0)
    pure $ BusEvent [] $ clocks instruction True
  -- INC16 r16
  INC16 r16 -> do
    writeR16 r16 =<< (+ 1) <$> readR16 r16
    pure $ BusEvent [] $ clocks instruction True
  -- DEC16 r16
  DEC16 r16 -> do
    writeR16 r16 =<< (subtract 1) <$> readR16 r16
    pure $ BusEvent [] $ clocks instruction True
  -- RLCA
  RLCA -> do
    a <- readR8 RegA
    setFlags $ if a .&. 0x80 /= 0 then flagCY else 0
    writeR8 RegA $ rotateL a 1
    pure $ BusEvent [] $ clocks instruction True
  -- RLA
  RLA -> do
    a <- readR8 RegA
    let ir = rotateL a 1
    hasCY <- testFlag flagCY
    setFlags $ if a .&. 0x80 /= 0 then flagCY else 0
    writeR8 RegA $ if hasCY then ir .|. 0x01 else ir .&. 0xFE
    pure $ BusEvent [] $ clocks instruction True
  -- RRCA
  RRCA -> do
    a <- readR8 RegA
    setFlags $ if a .&. 0x01 /= 0 then flagCY else 0
    writeR8 RegA $ rotateR a 1
    pure $ BusEvent [] $ clocks instruction True
  -- RRA
  RRA -> do
    a <- readR8 RegA
    let ir = rotateR a 1
    hasCY <- testFlag flagCY
    setFlags $ if a .&. 0x01 /= 0 then flagCY else 0
    writeR8 RegA $ if hasCY then ir .|. 0x80 else ir .&. 0x7F
    pure $ BusEvent [] $ clocks instruction True
  -- RLC \<r8|(HL)\>
  RLC so8 -> do
    value <- readSmallOperand8 so8
    setFlags $ (if value .&. 0x80 /= 0 then flagCY else 0) .|. (if value == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 $ rotateL value 1
    pure $ BusEvent writeAddr $ clocks instruction True
  -- RL \<r8|(HL)\>
  RL so8 -> do
    value <- readSmallOperand8 so8
    let ir = rotateL value 1
    hasCY <- testFlag flagCY
    let r = if hasCY then ir .|. 0x01 else ir .&. 0xFE
    setFlags $ (if value .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 r
    pure $ BusEvent writeAddr $ clocks instruction True
  -- RRC \<r8|(HL)\>
  RRC so8 -> do
    value <- readSmallOperand8 so8
    setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if value == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 $ rotateR value 1
    pure $ BusEvent writeAddr $ clocks instruction True
  -- RR \<r8|(HL)\>
  RR so8 -> do
    value <- readSmallOperand8 so8
    let ir = rotateR value 1
    hasCY <- testFlag flagCY
    let r = if hasCY then ir .|. 0x80 else ir .&. 0x7F
    setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 r
    pure $ BusEvent writeAddr $ clocks instruction True
  -- SLA \<r8|(HL)\>
  SLA so8 -> do
    value <- readSmallOperand8 so8
    let r = value `unsafeShiftL` 1
    setFlags $ (if value .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 r
    pure $ BusEvent writeAddr $ clocks instruction True
  -- SRA \<r8|(HL)\>
  SRA so8 -> do
    value <- readSmallOperand8 so8
    let r = (fromIntegral value `unsafeShiftR` 1) :: Int8
    setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 $ fromIntegral r
    pure $ BusEvent writeAddr $ clocks instruction True
  -- SRL \<r8|(HL)\>
  SRL so8 -> do
    value <- readSmallOperand8 so8
    let r = value `unsafeShiftR` 1
    setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 r
    pure $ BusEvent writeAddr $ clocks instruction True
  -- SWAP \<r8|(HL)\>
  SWAP so8 -> do
    value <- readSmallOperand8 so8
    setFlags $ if value == 0 then flagZ else 0
    writeAddr <- writeSmallOperand8 so8 $ (value `unsafeShiftR` 4) .|. (value `unsafeShiftL` 4)
    pure $ BusEvent writeAddr $ clocks instruction True
  -- BIT b \<r8|(HL)\>
  BIT w8 so8 -> do
    value <- readSmallOperand8 so8
    setFlagsMask (flagH .|. flagN .|. flagZ)
      $   flagH
      .|. (if value `testBit` fromIntegral w8 then 0 else flagZ)
    pure $ BusEvent [] $ clocks instruction True
  -- SET b \<r8|(HL)\>
  SET w8 so8 -> do
    value     <- readSmallOperand8 so8
    writeAddr <- writeSmallOperand8 so8 (value `setBit` fromIntegral w8)
    pure $ BusEvent writeAddr $ clocks instruction True
  -- RES b \<r8|(HL)\>
  RES w8 so8 -> do
    value     <- readSmallOperand8 so8
    writeAddr <- writeSmallOperand8 so8 (value `clearBit` fromIntegral w8)
    pure $ BusEvent writeAddr $ clocks instruction True
  -- JP im16
  JP w16 -> do
    writePC w16
    pure $ BusEvent [] $ clocks instruction True
  -- JP cc im16
  JPCC cc w16 -> do
    shouldJump <- testCondition cc
    when shouldJump $ writePC w16
    pure $ BusEvent [] $ clocks instruction shouldJump
  -- JR cc im8
  JR i8 -> do
    pc <- readPC
    writePC $ pc + fromIntegral i8
    pure $ BusEvent [] $ clocks instruction True
  -- JR cc im8
  JRCC cc i8 -> do
    shouldJump <- testCondition cc
    when shouldJump $ do
      pc <- readPC
      writePC $ pc + fromIntegral i8
    pure $ BusEvent [] $ clocks instruction shouldJump
  -- JP (HL)
  JPI -> do
    writePC =<< readR16 RegHL
    pure $ BusEvent [] $ clocks instruction True
  -- CALL im16
  CALL w16      -> doCall w16 $ clocks instruction True
  -- CALL cc im16
  CALLCC cc w16 -> do
    shouldCall <- testCondition cc
    if shouldCall
      then doCall w16 $ clocks instruction True
      else pure $ BusEvent [] $ clocks instruction False
  -- RETI
  RETI -> do
    sp  <- readR16 RegSP
    pcL <- fromIntegral <$> readByte sp
    pcH <- fromIntegral <$> readByte (sp + 1)
    writePC $ (pcH `unsafeShiftL` 8) .|. pcL
    writeR16 RegSP $ sp + 2
    setIME
    pure $ BusEvent [] $ clocks instruction True
  -- RET cc
  RET      -> doRet $ clocks instruction True
  -- RET cc
  RETCC cc -> do
    shouldCall <- testCondition cc
    if not shouldCall
      then pure $ BusEvent [] $ clocks instruction False
      else doRet $ clocks instruction True
  -- RST t
  RST w8 -> do
    sp <- readR16 RegSP
    pc <- readPC
    let sp' = sp - 2
    writeMem sp' pc
    writeR16 RegSP sp'
    writePC $ 8 * fromIntegral w8
    pure $ BusEvent [sp', sp' + 1] $ clocks instruction True
  -- DAA
  DAA -> do
    pc <- readPC
    error $ "DAA not implemented at " ++ formatHex pc
  -- CPL
  CPL -> do
    writeR8 RegA =<< complement <$> readR8 RegA
    setFlagsMask (flagH .|. flagN) (flagH .|. flagN)
    pure $ BusEvent [] $ clocks instruction True
  -- NOP
  NOP  -> pure $ BusEvent [] $ clocks instruction True
  -- HALT
  HALT -> do
    setMode ModeHalt
    pure $ BusEvent [] $ clocks instruction True
  -- STOP
  STOP -> do
    setMode ModeStop
    pure $ BusEvent [] $ clocks instruction True
  -- EI
  EI -> do
    setIME
    pure $ BusEvent [] $ clocks instruction True
  -- DI
  DI -> do
    clearIME
    pure $ BusEvent [] $ clocks instruction True
  -- CCF
  CCF -> do
    cf <- testFlag flagCY
    setFlagsMask (flagCY .|. flagH .|. flagN) $ if cf then 0 else flagCY
    pure $ BusEvent [] $ clocks instruction True
  -- SCF
  SCF -> do
    setFlagsMask (flagCY .|. flagH .|. flagN) flagCY
    pure $ BusEvent [] $ clocks instruction True
  -- INVALID instruction
  INVALID w8 -> do
    pc <- readPC
    error $ "Invalid instruction " ++ formatHex w8 ++ " at " ++ formatHex (pc - 1)

{-# INLINE doCall #-}
doCall :: UsesCPU env m => Word16 -> Int -> ReaderT env m BusEvent
doCall w16 numberOfClocks = do
  sp <- readR16 RegSP
  let sp' = sp - 2
  pc <- readPC
  writeMem sp' pc
  writePC w16
  writeR16 RegSP sp'
  pure $ BusEvent [sp', sp' + 1] numberOfClocks

{-# INLINE doRet #-}
doRet :: UsesCPU env m => Int -> ReaderT env m BusEvent
doRet numberOfClocks = do
  sp  <- readR16 RegSP
  pcL <- fromIntegral <$> readByte sp
  pcH <- fromIntegral <$> readByte (sp + 1)
  writePC $ (pcH `unsafeShiftL` 8) .|. pcL
  writeR16 RegSP $ sp + 2
  pure $ BusEvent [] numberOfClocks
