{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}

module GBC.CPU
  ( RegisterFile(..)
  , CPUMode(..)
  , CPUState(..)
  , HasCPU(..)
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
  , internalRegisters
  )
where

import           Common
import           Control.Exception              ( throwIO )
import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Int
import           Data.Maybe
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GBC.Decode
import           GBC.Errors
import           GBC.ISA
import           GBC.Memory
import           GBC.Mode
import           GBC.Registers

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
 , currentMode :: !CPUMode
} deriving (Eq, Ord, Show)

-- | The current CPU mode.
data CPUMode = ModeHalt | ModeStop | ModeNormal deriving (Eq, Ord, Show, Bounded, Enum)

-- | The internal CPU state.
data CPUState = CPUState {
    registers    :: !(ForeignPtr RegisterFile)
  , cpuMode      :: !(IORef CPUMode)
  , cpuType      :: !EmulatorMode
}

class HasMemory env => HasCPU env where
  forCPUState :: env -> CPUState

-- | Initialize a new CPU.
initCPU :: EmulatorMode -> IO CPUState
initCPU cpuType = do
  registers <- mallocForeignPtr 
  cpuMode <- newIORef ModeNormal
  pure CPUState {..}

-- | Get the current cpu mode.
{-# INLINABLE getMode #-}
getMode :: HasCPU env => ReaderT env IO CPUMode
getMode = do
  CPUState {..} <- asks forCPUState
  liftIO $ readIORef cpuMode

-- | Get the CPU mode.
{-# INLINABLE setMode #-}
setMode :: HasCPU env => CPUMode -> ReaderT env IO ()
setMode mode = do
  CPUState {..} <- asks forCPUState
  liftIO $ writeIORef cpuMode mode

-- | Get the values of all the registers.
{-# INLINABLE getRegisterFile #-}
getRegisterFile :: HasCPU env => ReaderT env IO RegisterFile
getRegisterFile = do
  CPUState {..} <- asks forCPUState
  liftIO $ withForeignPtr registers peek

-- | Read data from the register file.
{-# INLINE readRegister #-}
readRegister :: (HasCPU env, Storable a) => Int -> ReaderT env IO a
readRegister offset = do
  CPUState {..} <- asks forCPUState
  liftIO $ withForeignPtr registers $ flip peekByteOff offset

-- | Write data to the register file.
{-# INLINE writeRegister #-}
writeRegister :: (HasCPU env, Storable a) => Int -> a -> ReaderT env IO ()
writeRegister offset value = do
  CPUState {..} <- asks forCPUState
  liftIO $ withForeignPtr registers $ \ptr -> pokeByteOff ptr offset value

-- | Read a single register.
{-# INLINABLE readR8 #-}
readR8 :: HasCPU env => Register8 -> ReaderT env IO Word8
readR8 = readRegister . offsetR8

-- | Write a single register.
{-# INLINABLE writeR8 #-}
writeR8 :: HasCPU env => Register8 -> Word8 -> ReaderT env IO ()
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
readR16 :: HasCPU env => Register16 -> ReaderT env IO Word16
readR16 = readRegister . offsetR16

-- | Write a 16-bit register.
{-# INLINABLE writeR16 #-}
writeR16 :: HasCPU env => Register16 -> Word16 -> ReaderT env IO ()
writeR16 register = writeRegister $ offsetR16 register

-- | Get the offset in the register file of a register pair.
offsetR16 :: Register16 -> Int
offsetR16 RegBC = offsetC
offsetR16 RegDE = offsetE
offsetR16 RegHL = offsetL
offsetR16 RegSP = offsetSP

-- | Read the PC register.
{-# INLINABLE readPC #-}
readPC :: HasCPU env => ReaderT env IO Word16
readPC = readRegister offsetPC

-- | Write the PC register.
{-# INLINABLE writePC #-}
writePC :: HasCPU env => Word16 -> ReaderT env IO ()
writePC = writeRegister offsetPC

-- | Read the AF register.
{-# INLINABLE readAF #-}
readAF :: HasCPU env => ReaderT env IO Word16
readAF = readRegister offsetF

-- | Write the AF register.
{-# INLINABLE writeAF #-}
writeAF :: HasCPU env => Word16 -> ReaderT env IO ()
writeAF = writeRegister offsetF

-- | Read from a 'Operand8'. Returns a list of addresses that were read from.
{-# INLINE readOperand8 #-}
readOperand8 :: HasCPU env => Operand8 -> ReaderT env IO Word8
readOperand8 (R8 register) = readR8 register
readOperand8 (I8 value   ) = pure value
readOperand8 HLI           = readByte =<< readR16 RegHL

-- | Read from a 'SmallOperand8'.  Returns a list of addresses that were read from.
{-# INLINE readSmallOperand8 #-}
readSmallOperand8 :: HasCPU env => SmallOperand8 -> ReaderT env IO Word8
readSmallOperand8 (SmallR8 register) = readR8 register
readSmallOperand8 SmallHLI           = readOperand8 HLI

-- | Write to a 'SmallOperand8'.  Returns a list of addresses that were written to.
{-# INLINE writeSmallOperand8 #-}
writeSmallOperand8 :: HasCPU env => SmallOperand8 -> Word8 -> ReaderT env IO [Word16]
writeSmallOperand8 (SmallR8 register) value = writeR8 register value >> pure []
writeSmallOperand8 SmallHLI           value = do
  hl <- readR16 RegHL
  writeByte hl value
  pure [hl]

type Flag = Word8
flagZ, flagN, flagH, flagCY :: Flag
flagZ = 0x80
flagN = 0x40
flagH = 0x20
flagCY = 0x10

-- Master interrupt enable flag
{-# INLINABLE flagIME #-}
flagIME :: Word16
flagIME = 0x0100

-- | Check if a flag is set.
{-# INLINE testFlag #-}
testFlag :: HasCPU env => Flag -> ReaderT env IO Bool
testFlag flag = do
  f <- readRegister offsetF
  pure (f .&. flag /= 0)

-- | Check if a condition code is true.
{-# INLINE testCondition #-}
testCondition :: HasCPU env => ConditionCode -> ReaderT env IO Bool
testCondition CondNZ = not <$> testFlag flagZ
testCondition CondZ  = testFlag flagZ
testCondition CondNC = not <$> testFlag flagCY
testCondition CondC  = testFlag flagCY

-- | Read the F register.
{-# INLINE readF #-}
readF :: HasCPU env => ReaderT env IO Word8
readF = readRegister offsetF

-- | Write the F register.
{-# INLINE writeF #-}
writeF :: HasCPU env => Word8 -> ReaderT env IO ()
writeF = writeRegister offsetF

-- | Set all the flags.
{-# INLINE setFlags #-}
setFlags :: HasCPU env => Word8 -> ReaderT env IO ()
setFlags = setFlagsMask 0xF0

-- | Set some flags.
{-# INLINE setFlagsMask #-}
setFlagsMask
  :: HasCPU env
  => Word8 -- ^ bitmask containing flags to set.
  -> Word8 -- ^ new flags values.
  -> ReaderT env IO ()
setFlagsMask mask flags = do
  oldFlags <- readRegister offsetF
  writeRegister offsetF ((oldFlags .&. complement mask) .|. (flags .&. mask))

-- | Set the master interrupt flag.
{-# INLINE setIME #-}
setIME :: HasCPU env => ReaderT env IO ()
setIME = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .|. flagIME)

-- | Clear the master interrupt flag.
{-# INLINE clearIME #-}
clearIME :: HasCPU env => ReaderT env IO ()
clearIME = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .&. complement flagIME)

-- | Check the status of the interrupt flag.
{-# INLINE testIME #-}
testIME :: HasCPU env => ReaderT env IO Bool
testIME = do
  ime <- readRegister offsetHidden
  pure (ime .&. flagIME /= 0)

-- | Reset the CPU.
{-# INLINABLE reset #-}
reset :: HasCPU env => ReaderT env IO ()
reset = do
  CPUState {..} <- asks forCPUState

  writeR8 RegA (if cpuType == DMG then 0x01 else 0x11)
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
  writePC 0x100

  writeByte P1   0xFF
  writeByte TIMA 0x00
  writeByte TMA  0x00
  writeByte TAC  0x00
  writeByte NR10 0x80
  writeByte NR11 0xBF
  writeByte NR12 0xF3
  writeByte NR14 0xBF
  writeByte NR21 0x3F
  writeByte NR22 0x00
  writeByte NR24 0xBF
  writeByte NR30 0x7F
  writeByte NR31 0xFF
  writeByte NR32 0x9F
  writeByte NR33 0xBF
  writeByte NR41 0xFF
  writeByte NR42 0x00
  writeByte NR43 0x00
  writeByte NR30 0xBF
  writeByte NR50 0x77
  writeByte NR51 0xF3
  writeByte NR52 0xF1
  writeByte LCDC 0x91
  writeByte SCY  0x00
  writeByte SCX  0x00
  writeByte LYC  0x00
  writeByte BGP  0xFC
  writeByte OBP0 0xFF
  writeByte OBP1 0xFF
  writeByte WY   0x00
  writeByte WX   0x00
  writeByte IE   0x00
  writeByte IF   0x00

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
{-# INLINE decodeAndAdvancePC #-}
decodeAndAdvancePC :: HasCPU env => Decode a -> ReaderT env IO a
decodeAndAdvancePC action = do
  pc       <- readPC
  (r, pc') <- runDecode pc action
  writePC pc'
  pure r

-- | Decode an instruction.
{-# INLINE decodeOnly #-}
decodeOnly :: HasCPU env => Decode a -> ReaderT env IO a
decodeOnly action = do
  pc     <- readPC
  (r, _) <- runDecode pc action
  pure r

interruptVector :: Int -> Word16
interruptVector 0 = 0x40
interruptVector 1 = 0x48
interruptVector 2 = 0x50
interruptVector 3 = 0x58
interruptVector 4 = 0x60
interruptVector n = error ("Invalid interrupt vector " ++ show n)

-- | Get all of the pending interrupts that are ready to service.
{-# INLINE pendingEnabledInterrupts #-}
pendingEnabledInterrupts :: HasCPU env => ReaderT env IO Word8
pendingEnabledInterrupts = do
  interrupt <- readByte IF
  enabled   <- readByte IE
  pure (interrupt .&. enabled .&. 0x1F)

-- | Get the next interrupt to service.
{-# INLINE getNextInterrupt #-}
getNextInterrupt :: Word8 -> Int
getNextInterrupt = countTrailingZeros

-- | Raise an interrupt.
{-# INLINE raiseInterrupt #-}
raiseInterrupt :: HasMemory env => Int -> ReaderT env IO ()
raiseInterrupt interrupt = do
  rif <- readByte IF
  writeByte IF (rif `setBit` interrupt)

{-# INLINE push16 #-}
push16 :: HasCPU env => Word16 -> ReaderT env IO Word16
push16 value = do
  sp <- readR16 RegSP
  let sp' = sp - 2
  writeWord sp' value
  writeR16 RegSP sp'
  pure sp'

{-# INLINE pop16 #-}
pop16 :: HasCPU env => ReaderT env IO Word16
pop16 = do
  sp     <- readR16 RegSP
  valueL <- readByte sp
  valueH <- readByte (sp + 1)
  writeR16 RegSP (sp + 2)
  pure ((fromIntegral valueH `unsafeShiftL` 8) .|. fromIntegral valueL)

{-# INLINE arithOp8 #-}
arithOp8 :: HasCPU env => ArithmeticOp -> Operand8 -> Bool -> ReaderT env IO ()
arithOp8 op o8 carry = do
  a   <- readR8 RegA
  arg <- readOperand8 o8
  let (r, flags) = adder8 op a arg carry
  writeR8 RegA r
  setFlags flags

-- | Fetch, decode, and execute a single instruction.
{-# INLINABLE cpuStep #-}
cpuStep :: HasCPU env => ReaderT env IO BusEvent
cpuStep = do

  -- Check if we have an interrupt
  interrupts <- pendingEnabledInterrupts
  ime        <- testIME

  -- Deal with HALT mode
  modeRef    <- asks (cpuMode . forCPUState)
  mode       <- liftIO (readIORef modeRef)
  case mode of
    ModeNormal -> if interrupts /= 0 && ime
      then handleInterrupt interrupts
      else executeInstruction =<< decodeAndAdvancePC decode
    ModeHalt -> if interrupts /= 0
      then do
        liftIO (writeIORef modeRef ModeNormal)
        cpuStep
      else pure (BusEvent [] 32 ModeHalt)
    ModeStop -> if interrupts /= 0
      then do
        liftIO (writeIORef modeRef ModeNormal)
        cpuStep
      else pure (BusEvent [] 32 ModeHalt)

 where
  handleInterrupt interrupts = do
    -- Handle an interrupt
    let nextInterrupt = getNextInterrupt interrupts
    pc  <- readPC
    sp' <- push16 pc
    writePC (interruptVector nextInterrupt)
    clearIME
    writeByte IF (clearBit interrupts nextInterrupt)
    pure (BusEvent [sp', sp' + 1] 28 ModeNormal) -- TODO: Number of clocks here is just a guess

{-# INLINE noWrite #-}
noWrite :: Instruction -> BusEvent
noWrite instruction = BusEvent [] (clocks instruction True) ModeNormal

{-# INLINE noWriteLongClocks #-}
noWriteLongClocks :: Instruction -> Bool -> BusEvent
noWriteLongClocks instruction longClocks = BusEvent [] (clocks instruction longClocks) ModeNormal

{-# INLINE didWrite #-}
didWrite :: [Word16] -> Instruction -> BusEvent
didWrite addrs instruction = BusEvent addrs (clocks instruction True) ModeNormal

-- | Execute a single instruction.
{-# INLINABLE executeInstruction #-}
executeInstruction :: HasCPU env => Instruction -> ReaderT env IO BusEvent
executeInstruction instruction = case instruction of
  -- LD r8 \<r8|im8|(HL)\>
  LD_R8 r8 o8 -> do
    value <- readOperand8 o8
    writeR8 r8 value
    pure (noWrite instruction)
  -- LD (HL) r8
  LDHLI_R8 r8 -> do
    value <- readR8 r8
    hl    <- readR16 RegHL
    writeByte hl value
    pure (didWrite [hl] instruction)
  -- LD (HL) im8
  LDHLI_I8 im8 -> do
    hl <- readR16 RegHL
    writeByte hl im8
    pure (didWrite [hl] instruction)
  -- LD A (BC)
  LDA_BCI -> do
    bc    <- readR16 RegBC
    value <- readByte bc
    writeR8 RegA value
    pure (noWrite instruction)
  -- LD A (DE)
  LDA_DEI -> do
    de    <- readR16 RegDE
    value <- readByte de
    writeR8 RegA value
    pure (noWrite instruction)
  -- LD A (C)
  LDA_CI -> do
    c <- readR8 RegC
    let addr = fromIntegral c + 0xFF00
    value <- readByte addr
    writeR8 RegA value
    pure (noWrite instruction)
  -- LD (C) A
  LDCI_A -> do
    c <- readR8 RegC
    let addr = fromIntegral c + 0xFF00
    value <- readR8 RegA
    writeByte addr value
    pure (didWrite [addr] instruction)
  -- LD A (im8)
  LDA_I8I w8 -> do
    let addr = fromIntegral w8 + 0xFF00
    value <- readByte addr
    writeR8 RegA value
    pure (noWrite instruction)
  -- LD (im8) A
  LDI8I_A w8 -> do
    let addr = fromIntegral w8 + 0xFF00
    value <- readR8 RegA
    writeByte addr value
    pure (didWrite [addr] instruction)
  -- LD A (im16)
  LDA_I16I w16 -> do
    value <- readByte w16
    writeR8 RegA value
    pure (noWrite instruction)
  -- LD (im16) A
  LDI16I_A w16 -> do
    value <- readR8 RegA
    writeByte w16 value
    pure (didWrite [w16] instruction)
  -- LD A (HL++)
  LDA_INC -> do
    hl    <- readR16 RegHL
    value <- readByte hl
    writeR8 RegA value
    writeR16 RegHL (hl + 1)
    pure (noWrite instruction)
  -- LD A (HL--)
  LDA_DEC -> do
    hl    <- readR16 RegHL
    value <- readByte hl
    writeR8 RegA value
    writeR16 RegHL (hl - 1)
    pure (noWrite instruction)
  -- LD (BC) A
  LDBCI_A -> do
    value <- readR8 RegA
    addr  <- readR16 RegBC
    writeByte addr value
    pure (didWrite [addr] instruction)
  -- LD (DE) A
  LDDEI_A -> do
    value <- readR8 RegA
    addr  <- readR16 RegDE
    writeByte addr value
    pure (didWrite [addr] instruction)
  -- LD (HL++) A
  LDHLI_INC -> do
    value <- readR8 RegA
    hl    <- readR16 RegHL
    writeByte hl value
    writeR16 RegHL (hl + 1)
    pure (didWrite [hl] instruction)
  -- LD (HL--) A
  LDHLI_DEC -> do
    value <- readR8 RegA
    hl    <- readR16 RegHL
    writeByte hl value
    writeR16 RegHL (hl - 1)
    pure (didWrite [hl] instruction)
  -- LD r16 im16
  LD16_I16 r16 w16 -> do
    writeR16 r16 w16
    pure (noWrite instruction)
  -- LD SP HL
  LDSP -> do
    writeR16 RegSP =<< readR16 RegHL
    pure (noWrite instruction)
  -- PUSH r16
  PUSH r16 -> do
    value <- if r16 == RegSP then readAF else readR16 r16
    sp'   <- push16 value
    pure (didWrite [sp', sp' + 1] instruction)
  -- POP r16
  POP r16 -> do
    value <- pop16
    if r16 == RegSP then writeAF (value .&. 0xFFF0) else writeR16 r16 value
    pure (noWrite instruction)
  -- LDHL SP im8
  LDHL i8 -> do
    sp <- fromIntegral <$> readR16 RegSP
    let wi8     = fromIntegral i8 :: Int32
    let wr      = sp + wi8
    let carryH = (sp .&. 0x00000010) `xor` (wi8 .&. 0x00000010) /= (wr .&. 0x00000010)
    let carryCY = (sp .&. 0x00000100) `xor` (wi8 .&. 0x00000100) /= (wr .&. 0x00000100)
    writeR16 RegHL (fromIntegral wr)
    setFlags ((if carryCY then flagCY else 0) .|. (if carryH then flagH else 0))
    pure (noWrite instruction)
  -- LD (im16) SP
  LDI16I_SP w16 -> do
    sp <- readR16 RegSP
    writeWord w16 sp
    pure (didWrite [w16, w16 + 1] instruction)
  -- ADD \<r8|im8|(HL)\>
  ADD o8 -> do
    arithOp8 OpAdd o8 False
    pure (noWrite instruction)
  -- ADC \<r8|im8|(HL)\>
  ADC o8 -> do
    carry <- testFlag flagCY
    arithOp8 OpAdd o8 carry
    pure (noWrite instruction)
  -- SUB \<r8|im8|(HL)\>
  SUB o8 -> do
    arithOp8 OpSub o8 False
    pure (noWrite instruction)
  -- SBC \<r8|im8|(HL)\>
  SBC o8 -> do
    carry <- testFlag flagCY
    arithOp8 OpSub o8 carry
    pure (noWrite instruction)
  -- AND \<r8|im8|(HL)\>
  AND o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let r = a .&. arg
    writeR8 RegA r
    setFlags (flagH .|. (if r == 0 then flagZ else 0))
    pure (noWrite instruction)
  -- OR \<r8|im8|(HL)\>
  OR o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let r = a .|. arg
    writeR8 RegA r
    setFlags (if r == 0 then flagZ else 0)
    pure (noWrite instruction)
  -- XOR \<r8|im8|(HL)\>
  XOR o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let r = a `xor` arg
    writeR8 RegA r
    setFlags (if r == 0 then flagZ else 0)
    pure (noWrite instruction)
  -- CP \<r8|im8|(HL)\>
  CP o8 -> do
    a   <- readR8 RegA
    arg <- readOperand8 o8
    let (_, flags) = adder8 OpSub a arg False
    setFlags flags
    pure (noWrite instruction)
  -- INC \<r8|(HL)\>
  INC so8 -> do
    value <- readSmallOperand8 so8
    let (r, flags) = inc8 OpAdd value
    wroteAddr <- writeSmallOperand8 so8 r
    setFlagsMask (flagH .|. flagN .|. flagZ) flags
    pure (didWrite wroteAddr instruction)
  -- DEC \<r8|(HL)\>
  DEC so8 -> do
    value <- readSmallOperand8 so8
    let (r, flags) = inc8 OpSub value
    wroteAddr <- writeSmallOperand8 so8 r
    setFlagsMask (flagH .|. flagN .|. flagZ) flags
    pure (didWrite wroteAddr instruction)
  -- ADD HL r16
  ADDHL r16 -> do
    hl <- readR16 RegHL
    ss <- readR16 r16
    let hl'     = fromIntegral hl
    let ss'     = fromIntegral ss
    let wr      = hl' + ss' :: Word32
    let carryH = (hl' .&. 0x00001000) `xor` (ss' .&. 0x00001000) /= (wr .&. 0x00001000)
    let carryCY = (hl' .&. 0x00010000) `xor` (ss' .&. 0x00010000) /= (wr .&. 0x00010000)
    writeR16 RegHL (fromIntegral wr)
    setFlagsMask (flagCY .|. flagH .|. flagN)
                 ((if carryH then flagH else 0) .|. (if carryCY then flagCY else 0))
    pure (noWrite instruction)
  -- ADD SP im8
  ADDSP i8 -> do
    sp <- readR16 RegSP
    let sp'     = fromIntegral sp
    let i8'     = fromIntegral i8
    let wr      = i8' + sp' :: Int32
    let carryH = (sp' .&. 0x00000010) `xor` (i8' .&. 0x00000010) /= (wr .&. 0x00000010)
    let carryCY = (sp' .&. 0x00000100) `xor` (i8' .&. 0x00000100) /= (wr .&. 0x00000100)
    writeR16 RegSP (fromIntegral (wr .&. 0xFFFF))
    setFlags ((if carryH then flagH else 0) .|. (if carryCY then flagCY else 0))
    pure (noWrite instruction)
  -- INC16 r16
  INC16 r16 -> do
    value <- readR16 r16
    writeR16 r16 (value + 1)
    pure (noWrite instruction)
  -- DEC16 r16
  DEC16 r16 -> do
    value <- readR16 r16
    writeR16 r16 (value - 1)
    pure (noWrite instruction)
  -- RLCA
  RLCA -> do
    a <- readR8 RegA
    setFlags (if a .&. 0x80 /= 0 then flagCY else 0)
    writeR8 RegA (rotateL a 1)
    pure (noWrite instruction)
  -- RLA
  RLA -> do
    a <- readR8 RegA
    let ir = rotateL a 1
    hasCY <- testFlag flagCY
    setFlags (if a .&. 0x80 /= 0 then flagCY else 0)
    writeR8 RegA (if hasCY then ir .|. 0x01 else ir .&. 0xFE)
    pure (noWrite instruction)
  -- RRCA
  RRCA -> do
    a <- readR8 RegA
    setFlags (if a .&. 0x01 /= 0 then flagCY else 0)
    writeR8 RegA (rotateR a 1)
    pure (noWrite instruction)
  -- RRA
  RRA -> do
    a <- readR8 RegA
    let ir = rotateR a 1
    hasCY <- testFlag flagCY
    setFlags (if a .&. 0x01 /= 0 then flagCY else 0)
    writeR8 RegA (if hasCY then ir .|. 0x80 else ir .&. 0x7F)
    pure (noWrite instruction)
  -- RLC \<r8|(HL)\>
  RLC so8 -> do
    value <- readSmallOperand8 so8
    setFlags ((if value .&. 0x80 /= 0 then flagCY else 0) .|. (if value == 0 then flagZ else 0))
    writeAddr <- writeSmallOperand8 so8 (rotateL value 1)
    pure (didWrite writeAddr instruction)
  -- RL \<r8|(HL)\>
  RL so8 -> do
    value <- readSmallOperand8 so8
    let ir = rotateL value 1
    hasCY <- testFlag flagCY
    let r = if hasCY then ir .|. 0x01 else ir .&. 0xFE
    setFlags ((if value .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
    writeAddr <- writeSmallOperand8 so8 r
    pure (didWrite writeAddr instruction)
  -- RRC \<r8|(HL)\>
  RRC so8 -> do
    value <- readSmallOperand8 so8
    setFlags ((if value .&. 0x01 /= 0 then flagCY else 0) .|. (if value == 0 then flagZ else 0))
    writeAddr <- writeSmallOperand8 so8 (rotateR value 1)
    pure (didWrite writeAddr instruction)
  -- RR \<r8|(HL)\>
  RR so8 -> do
    value <- readSmallOperand8 so8
    let ir = rotateR value 1
    hasCY <- testFlag flagCY
    let r = if hasCY then ir .|. 0x80 else ir .&. 0x7F
    setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 r
    pure (didWrite writeAddr instruction)
  -- SLA \<r8|(HL)\>
  SLA so8 -> do
    value <- readSmallOperand8 so8
    let r = value `unsafeShiftL` 1
    setFlags ((if value .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
    writeAddr <- writeSmallOperand8 so8 r
    pure (didWrite writeAddr instruction)
  -- SRA \<r8|(HL)\>
  SRA so8 -> do
    value <- readSmallOperand8 so8
    let r = (fromIntegral value `unsafeShiftR` 1) :: Int8
    setFlags ((if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
    writeAddr <- writeSmallOperand8 so8 (fromIntegral r)
    pure (didWrite writeAddr instruction)
  -- SRL \<r8|(HL)\>
  SRL so8 -> do
    value <- readSmallOperand8 so8
    let r = value `unsafeShiftR` 1
    setFlags ((if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
    writeAddr <- writeSmallOperand8 so8 r
    pure (didWrite writeAddr instruction)
  -- SWAP \<r8|(HL)\>
  SWAP so8 -> do
    value <- readSmallOperand8 so8
    setFlags (if value == 0 then flagZ else 0)
    writeAddr <- writeSmallOperand8 so8 ((value `unsafeShiftR` 4) .|. (value `unsafeShiftL` 4))
    pure (didWrite writeAddr instruction)
  -- BIT b \<r8|(HL)\>
  BIT w8 so8 -> do
    value <- readSmallOperand8 so8
    setFlagsMask (flagH .|. flagN .|. flagZ)
                 (flagH .|. (if value `testBit` fromIntegral w8 then 0 else flagZ))
    pure (noWrite instruction)
  -- SET b \<r8|(HL)\>
  SET w8 so8 -> do
    value     <- readSmallOperand8 so8
    writeAddr <- writeSmallOperand8 so8 (value `setBit` fromIntegral w8)
    pure (didWrite writeAddr instruction)
  -- RES b \<r8|(HL)\>
  RES w8 so8 -> do
    value     <- readSmallOperand8 so8
    writeAddr <- writeSmallOperand8 so8 (value `clearBit` fromIntegral w8)
    pure (didWrite writeAddr instruction)
  -- JP im16
  JP w16 -> do
    writePC w16
    pure (noWrite instruction)
  -- JP cc im16
  JPCC cc w16 -> do
    shouldJump <- testCondition cc
    when shouldJump (writePC w16)
    pure (noWriteLongClocks instruction shouldJump)
  -- JR cc im8
  JR i8 -> do
    pc <- readPC
    writePC (pc + fromIntegral i8)
    pure (noWrite instruction)
  -- JR cc im8
  JRCC cc i8 -> do
    shouldJump <- testCondition cc
    when shouldJump $ do
      pc <- readPC
      writePC (pc + fromIntegral i8)
    pure (noWriteLongClocks instruction shouldJump)
  -- JP (HL)
  JPI -> do
    writePC =<< readR16 RegHL
    pure (noWrite instruction)
  -- CALL im16
  CALL w16      -> doCall w16 (clocks instruction True)
  -- CALL cc im16
  CALLCC cc w16 -> do
    shouldCall <- testCondition cc
    if shouldCall
      then doCall w16 (clocks instruction True)
      else pure (noWriteLongClocks instruction False)
  -- RETI
  RETI -> do
    writePC =<< pop16
    setIME
    pure (noWrite instruction)
  -- RET cc
  RET      -> doRet (clocks instruction True)
  -- RET cc
  RETCC cc -> do
    shouldCall <- testCondition cc
    if not shouldCall
      then pure (noWriteLongClocks instruction False)
      else doRet (clocks instruction True)
  -- RST t
  RST w8 -> do
    pc  <- readPC
    sp' <- push16 pc
    writePC $ 8 * fromIntegral w8
    pure (didWrite [sp', sp' + 1] instruction)
  -- DAA
  DAA -> do
    flags <- readF
    a     <- readR8 RegA
    let isH   = isFlagSet flagH flags
    let isN   = isFlagSet flagN flags
    let isCy  = isFlagSet flagCY flags
    let aWide = fromIntegral a :: Int

    let rWide = if isN
          then
            let aWide' = if isH then (aWide - 0x06) .&. 0xFF else aWide
            in  if isCy then aWide' - 0x60 else aWide'
          else
            let aWide' = if isH || aWide .&. 0x0F > 9 then aWide + 0x06 else aWide
            in  if isCy || aWide' > 0x9F then aWide' + 0x60 else aWide'

    let r = fromIntegral (rWide .&. 0xFF)
    writeR8 RegA r
    setFlagsMask
      (flagCY .|. flagZ .|. flagH)
      ((if isCy || rWide .&. 0x100 == 0x100 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
    pure (noWrite instruction)
  -- CPL
  CPL -> do
    a <- readR8 RegA
    writeR8 RegA (complement a)
    setFlagsMask (flagH .|. flagN) (flagH .|. flagN)
    pure (noWrite instruction)
  -- NOP
  NOP  -> pure (noWrite instruction)
  -- HALT
  HALT -> do
    setMode ModeHalt
    pure (BusEvent [] (clocks instruction True) ModeHalt)
  -- STOP
  STOP -> do
    liftIO (putStrLn "STOP")
    key1 <- readByte KEY1
    if key1 `testBit` 0
      then do
        writeByte KEY1 0
        pure (BusEvent [] (clocks instruction True) ModeNormal)
      else do
        setMode ModeStop
        pure (BusEvent [] (clocks instruction True) ModeStop)
  -- EI
  EI -> do
    setIME
    pure (noWrite instruction)
  -- DI
  DI -> do
    clearIME
    pure (noWrite instruction)
  -- CCF
  CCF -> do
    cf <- testFlag flagCY
    setFlagsMask (flagCY .|. flagH .|. flagN) (if cf then 0 else flagCY)
    pure (noWrite instruction)
  -- SCF
  SCF -> do
    setFlagsMask (flagCY .|. flagH .|. flagN) flagCY
    pure (noWrite instruction)
  -- INVALID instruction
  INVALID w8 -> liftIO (throwIO (InvalidInstruction w8))

{-# INLINE doCall #-}
doCall :: HasCPU env => Word16 -> Int -> ReaderT env IO BusEvent
doCall w16 numberOfClocks = do
  pc  <- readPC
  sp' <- push16 pc
  writePC w16
  pure (BusEvent [sp', sp' + 1] numberOfClocks ModeNormal)

{-# INLINE doRet #-}
doRet :: HasCPU env => Int -> ReaderT env IO BusEvent
doRet numberOfClocks = do
  writePC =<< pop16
  pure (BusEvent [] numberOfClocks ModeNormal)

internalRegisters :: HasMemory env => ReaderT env IO [RegisterInfo]
internalRegisters = do
  p1   <- readByte P1
  rif  <- readByte IF
  rie  <- readByte IE
  svbk <- readByte SVBK
  key1 <- readByte KEY1
  pure
    [ RegisterInfo P1   "P1"   p1   []
    , RegisterInfo IF   "IF"   rif  (decodeInterrupts rif "Pending")
    , RegisterInfo IE   "IE"   rie  (decodeInterrupts rie "Enabled")
    , RegisterInfo SVBK "SVBK" svbk (decodeSVBK svbk)
    , RegisterInfo
      KEY1
      "KEY1"
      key1
      [ ("Double Speed"          , show $ key1 `testBit` 7)
      , ("Enable Speed Switching", show $ key1 `testBit` 0)
      ]
    ]
 where
  decodeInterrupts i s = map (, s) $ catMaybes
    [ if i `testBit` 0 then Just "Interrupt 40 VBlank" else Nothing
    , if i `testBit` 1 then Just "Interrupt 48 LCDC Status" else Nothing
    , if i `testBit` 2 then Just "Interrupt 50 Timer" else Nothing
    , if i `testBit` 3 then Just "Interrupt 58 Serial Transfer" else Nothing
    , if i `testBit` 4 then Just "Interrupt 60 Keypad" else Nothing
    ]
  decodeSVBK svbk = let bank = svbk .&. 7 in [("RAM Bank", show $ if bank == 0 then 1 else bank)]
