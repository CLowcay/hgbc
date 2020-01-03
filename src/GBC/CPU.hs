{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}

module GBC.CPU
  ( RegisterFile(..)
  , CPUMode(..)
  , CPUState(..)
  , HasCPU(..)
  , initCPU
  , cpuPorts
  , getMode
  , getCPUCycleClocks
  , reset
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
  , flagDoubleSpeed
  , testCondition
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
import           GBC.CPU.Decode
import           GBC.CPU.ISA
import           GBC.Errors
import           GBC.Interrupts
import           GBC.Memory
import           GBC.Mode
import           GBC.Primitive
import           GBC.Primitive.UnboxedRef
import           GBC.Registers
import qualified Data.Vector                   as V

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

-- | The current CPU mode.
data CPUMode = ModeHalt | ModeStop | ModeNormal deriving (Eq, Ord, Show, Bounded, Enum)

-- | The internal CPU state.
data CPUState = CPUState {
    cpuType        :: !EmulatorMode
  , registers      :: !(ForeignPtr RegisterFile)
  , portIF         :: !(Port Word8)
  , portIE         :: !(Port Word8)
  , portKEY1       :: !(Port Word8)
  , cpuMode        :: !(IORef CPUMode)
  , cpuCycleClocks :: !(UnboxedRef Int)
}

class HasMemory env => HasCPU env where
  forCPUState :: env -> CPUState

-- | Initialize a new CPU.
initCPU :: Port Word8 -> Port Word8 -> EmulatorMode -> IO CPUState
initCPU portIF portIE cpuType = do
  registers      <- mallocForeignPtr
  portKEY1       <- newPort 0x00 0x01 alwaysUpdate
  cpuMode        <- newIORef ModeNormal
  cpuCycleClocks <- newUnboxedRef 4
  pure CPUState { .. }

cpuPorts :: CPUState -> [(Word16, Port Word8)]
cpuPorts CPUState {..} = [(KEY1, portKEY1)]

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
readR8 :: HasCPU env => RegisterR -> ReaderT env IO Word8
readR8 = readRegister . offsetR8

-- | Write a single register.
{-# INLINABLE writeR8 #-}
writeR8 :: HasCPU env => RegisterR -> Word8 -> ReaderT env IO ()
writeR8 register = writeRegister $ offsetR8 register

-- | Get the offset in the register file of a single register.
offsetR8 :: RegisterR -> Int
offsetR8 RegA = offsetA
offsetR8 RegB = offsetB
offsetR8 RegC = offsetC
offsetR8 RegD = offsetD
offsetR8 RegE = offsetE
offsetR8 RegH = offsetH
offsetR8 RegL = offsetL

-- | Read a 16-bit register.
{-# INLINABLE readR16 #-}
readR16 :: HasCPU env => RegisterSS -> ReaderT env IO Word16
readR16 = readRegister . offsetR16

-- | Write a 16-bit register.
{-# INLINABLE writeR16 #-}
writeR16 :: HasCPU env => RegisterSS -> Word16 -> ReaderT env IO ()
writeR16 register = writeRegister $ offsetR16 register

-- | Get the offset in the register file of a register pair.
offsetR16 :: RegisterSS -> Int
offsetR16 RegBC = offsetC
offsetR16 RegDE = offsetE
offsetR16 RegHL = offsetL
offsetR16 RegSP = offsetSP

-- | Read a 16-bit register.
{-# INLINABLE readR16qq #-}
readR16qq :: HasCPU env => RegisterQQ -> ReaderT env IO Word16
readR16qq = readRegister . offsetR16qq

-- | Write a 16-bit register.
{-# INLINABLE writeR16qq #-}
writeR16qq :: HasCPU env => RegisterQQ -> Word16 -> ReaderT env IO ()
writeR16qq register = writeRegister $ offsetR16qq register

-- | Get the offset in the register file of a register pair.
offsetR16qq :: RegisterQQ -> Int
offsetR16qq PushPopBC = offsetC
offsetR16qq PushPopDE = offsetE
offsetR16qq PushPopHL = offsetL
offsetR16qq PushPopAF = offsetF

-- | Read the PC register.
{-# INLINABLE readPC #-}
readPC :: HasCPU env => ReaderT env IO Word16
readPC = readRegister offsetPC

-- | Write the PC register.
{-# INLINABLE writePC #-}
writePC :: HasCPU env => Word16 -> ReaderT env IO ()
writePC = writeRegister offsetPC

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
  oldFlags <- readF
  writeF ((oldFlags .&. complement mask) .|. (flags .&. mask))

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
  writeByte NR10 0x00
  writeByte NR11 0xBF
  writeByte NR12 0xF3
  writeByte NR13 0x00
  writeByte NR14 0x00
  writeByte NR21 0x00
  writeByte NR22 0x00
  writeByte NR23 0x00
  writeByte NR24 0x00
  writeByte NR30 0x00
  writeByte NR31 0x00
  writeByte NR32 0x00
  writeByte NR33 0x00
  writeByte NR34 0x00
  writeByte NR41 0x00
  writeByte NR42 0x00
  writeByte NR43 0x00
  writeByte NR44 0x00
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
      -- TODO: simplify?
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

flagDoubleSpeed :: Word8
flagDoubleSpeed = 0x80

flagSpeedSwitch :: Word8
flagSpeedSwitch = 0x01

interruptVector :: Interrupt -> Word16
interruptVector InterruptVBlank            = 0x40
interruptVector InterruptLCDCStat          = 0x48
interruptVector InterruptTimerOverflow     = 0x50
interruptVector InterruptEndSerialTransfer = 0x58
interruptVector InterruptP1Low             = 0x60

-- | Fetch, decode, and execute a single instruction.
{-# INLINABLE cpuStep #-}
cpuStep :: HasCPU env => ReaderT env IO Int
cpuStep = do
  CPUState {..} <- asks forCPUState

  -- Check if we have an interrupt
  interrupts    <- liftIO $ pendingEnabledInterrupts portIF portIE
  ime           <- testIME

  -- Deal with HALT mode
  mode          <- liftIO (readIORef cpuMode)
  case mode of
    ModeNormal -> if interrupts /= 0 && ime then handleInterrupt interrupts else executeInstruction
    ModeHalt   -> if interrupts /= 0
      then do
        liftIO (writeIORef cpuMode ModeNormal)
        cpuStep
      else pure 8
    ModeStop -> if interrupts /= 0
      then do
        liftIO (writeIORef cpuMode ModeNormal)
        cpuStep
      else pure 8

 where
  handleInterrupt interrupts = do
    -- Handle an interrupt
    let nextInterrupt = getNextInterrupt interrupts
    pc <- readPC
    push16 pc
    writePC (interruptVector nextInterrupt)
    clearIME
    CPUState {..} <- asks forCPUState
    liftIO $ clearInterrupt portIF nextInterrupt
    pure 7  -- TODO: Number of clocks here is just a guess

{-# INLINE getCPUCycleClocks #-}
getCPUCycleClocks :: HasCPU env => ReaderT env IO Int
getCPUCycleClocks = do
  CPUState {..} <- asks forCPUState
  liftIO (readUnboxedRef cpuCycleClocks)

{-# SPECIALIZE table0 :: HasCPU env => V.Vector (CPUM env Int) #-}
{-# SPECIALIZE table1 :: HasCPU env => V.Vector (CPUM env Int) #-}
{-# SPECIALIZE fetchAndExecute :: HasCPU env => CPUM env Int #-}
{-# INLINABLE executeInstruction #-}
executeInstruction :: HasCPU env => ReaderT env IO Int
executeInstruction = runCPUM fetchAndExecute

newtype CPUM env a = CPUM {runCPUM :: ReaderT env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance HasCPU env => MonadFetch (CPUM env) where
  nextByte = CPUM $ do
    pc <- readPC
    writePC (pc + 1)
    readByte pc
  nextWord = CPUM $ do
    pc <- readPC
    writePC (pc + 2)
    readWord pc

instance HasCPU env => MonadGMBZ80 (CPUM env) where
  type ExecuteResult (CPUM env) = Int
  ldrr r r' = CPUM $ do
    writeR8 r =<< readR8 r'
    pure 1
  ldrn r n = CPUM $ do
    writeR8 r n
    pure 2
  ldrHL r = CPUM $ do
    hl <- readR16 RegHL
    writeR8 r =<< readByte hl
    pure 2
  ldHLr r = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< readR8 r
    pure 2
  ldHLn n = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl n
    pure 3
  ldaBC = CPUM $ do
    bc <- readR16 RegBC
    writeR8 RegA =<< readByte bc
    pure 2
  ldaDE = CPUM $ do
    de <- readR16 RegDE
    writeR8 RegA =<< readByte de
    pure 2
  ldaC = CPUM $ do
    c <- readR8 RegC
    writeR8 RegA =<< readByte (0xFF00 + fromIntegral c)
    pure 2
  ldCa = CPUM $ do
    c <- readR8 RegC
    writeByte (0xFF00 + fromIntegral c) =<< readR8 RegA
    pure 2
  ldan n = CPUM $ do
    writeR8 RegA =<< readByte (0xFF00 + fromIntegral n)
    pure 3
  ldna n = CPUM $ do
    writeByte (0xFF00 + fromIntegral n) =<< readR8 RegA
    pure 3
  ldann nn = CPUM $ do
    writeR8 RegA =<< readByte nn
    pure 4
  ldnna nn = CPUM $ do
    writeByte nn =<< readR8 RegA
    pure 4
  ldaHLI = CPUM $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl + 1)
    writeR8 RegA =<< readByte hl
    pure 2
  ldaHLD = CPUM $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl - 1)
    writeR8 RegA =<< readByte hl
    pure 2
  ldBCa = CPUM $ do
    bc <- readR16 RegBC
    writeByte bc =<< readR8 RegA
    pure 2
  ldDEa = CPUM $ do
    de <- readR16 RegDE
    writeByte de =<< readR8 RegA
    pure 2
  ldHLIa = CPUM $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl + 1)
    writeByte hl =<< readR8 RegA
    pure 2
  ldHLDa = CPUM $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl - 1)
    writeByte hl =<< readR8 RegA
    pure 2
  ldddnn dd nn = CPUM $ do
    writeR16 dd nn
    pure 3
  ldSPHL = CPUM $ do
    writeR16 RegSP =<< readR16 RegHL
    pure 2
  push qq = CPUM $ do
    push16 =<< readR16qq qq
    pure 4
  pop qq = CPUM $ do
    writeR16qq qq =<< pop16
    pure 3
  ldhl i = CPUM $ do
    sp <- fromIntegral <$> readR16 RegSP
    let wi      = fromIntegral i :: Int32
    let wr      = sp + wi
    let carryH = (sp .&. 0x00000010) `xor` (wi .&. 0x00000010) /= (wr .&. 0x00000010)
    let carryCY = (sp .&. 0x00000100) `xor` (wi .&. 0x00000100) /= (wr .&. 0x00000100)
    writeR16 RegHL (fromIntegral wr)
    setFlags ((if carryCY then flagCY else 0) .|. (if carryH then flagH else 0))
    pure 3
  ldnnSP nn = CPUM $ do
    writeWord nn =<< readR16 RegSP
    pure 5
  addr r = CPUM $ do
    v <- readR8 r
    arithOp8 OpAdd v False
    pure 1
  addn n = CPUM $ do
    arithOp8 OpAdd n False
    pure 2
  addhl = CPUM $ do
    v <- readByte =<< readR16 RegHL
    arithOp8 OpAdd v False
    pure 2
  adcr r = CPUM $ do
    v <- readR8 r
    arithOp8 OpAdd v True
    pure 1
  adcn n = CPUM $ do
    arithOp8 OpAdd n True
    pure 2
  adchl = CPUM $ do
    v <- readByte =<< readR16 RegHL
    arithOp8 OpAdd v True
    pure 2
  subr r = CPUM $ do
    v <- readR8 r
    arithOp8 OpSub v False
    pure 1
  subn n = CPUM $ do
    arithOp8 OpSub n False
    pure 2
  subhl = CPUM $ do
    v <- readByte =<< readR16 RegHL
    arithOp8 OpSub v False
    pure 2
  sbcr r = CPUM $ do
    v <- readR8 r
    arithOp8 OpSub v True
    pure 1
  sbcn n = CPUM $ do
    arithOp8 OpSub n True
    pure 2
  sbchl = CPUM $ do
    v <- readByte =<< readR16 RegHL
    arithOp8 OpSub v True
    pure 2
  andr r = CPUM $ do
    andOp8 =<< readR8 r
    pure 1
  andn n = CPUM $ do
    andOp8 n
    pure 2
  andhl = CPUM $ do
    andOp8 =<< readByte =<< readR16 RegHL
    pure 2
  orr r = CPUM $ do
    orOp8 =<< readR8 r
    pure 1
  orn n = CPUM $ do
    orOp8 n
    pure 2
  orhl = CPUM $ do
    orOp8 =<< readByte =<< readR16 RegHL
    pure 2
  xorr r = CPUM $ do
    xorOp8 =<< readR8 r
    pure 1
  xorn n = CPUM $ do
    xorOp8 n
    pure 2
  xorhl = CPUM $ do
    xorOp8 =<< readByte =<< readR16 RegHL
    pure 2
  cpr r = CPUM $ do
    a <- readR8 RegA
    v <- readR8 r
    let (_, flags) = adder8 OpSub a v False
    setFlags flags
    pure 1
  cpn n = CPUM $ do
    a <- readR8 RegA
    let (_, flags) = adder8 OpSub a n False
    setFlags flags
    pure 2
  cphl = CPUM $ do
    a <- readR8 RegA
    v <- readByte =<< readR16 RegHL
    let (_, flags) = adder8 OpSub a v False
    setFlags flags
    pure 2
  incr r = CPUM $ do
    v <- readR8 r
    let (v', flags) = inc8 OpAdd v
    writeR8 r v'
    setFlags flags
    pure 1
  inchl = CPUM $ do
    hl <- readR16 RegHL
    v  <- readByte hl
    let (v', flags) = inc8 OpAdd v
    setFlags flags
    writeByte hl v'
    pure 3
  decr r = CPUM $ do
    v <- readR8 r
    let (v', flags) = inc8 OpSub v
    writeR8 r v'
    setFlags flags
    pure 1
  dechl = CPUM $ do
    hl <- readR16 RegHL
    v  <- readByte hl
    let (v', flags) = inc8 OpSub v
    setFlags flags
    writeByte hl v'
    pure 3
  addhlss ss = CPUM $ do
    hl <- readR16 RegHL
    v  <- readR16 ss
    let hl'     = fromIntegral hl
    let v'      = fromIntegral v
    let wr      = hl' + v' :: Word32
    let carryH = (hl' .&. 0x00001000) `xor` (v' .&. 0x00001000) /= (wr .&. 0x00001000)
    -- TODO: simplify?
    let carryCY = (hl' .&. 0x00010000) `xor` (v' .&. 0x00010000) /= (wr .&. 0x00010000)
    writeR16 RegHL (fromIntegral wr)
    setFlagsMask (flagCY .|. flagH .|. flagN)
                 ((if carryH then flagH else 0) .|. (if carryCY then flagCY else 0))
    pure 2
  addSP e = CPUM $ do
    sp <- readR16 RegSP
    let sp'     = fromIntegral sp
    let e'      = fromIntegral e
    let wr      = e' + sp' :: Int32
    let carryH = (sp' .&. 0x00000010) `xor` (e' .&. 0x00000010) /= (wr .&. 0x00000010)
    -- TODO: simplify?
    let carryCY = (sp' .&. 0x00000100) `xor` (e' .&. 0x00000100) /= (wr .&. 0x00000100)
    writeR16 RegSP (fromIntegral (wr .&. 0xFFFF))
    setFlags ((if carryH then flagH else 0) .|. (if carryCY then flagCY else 0))
    pure 4
  incss ss = CPUM $ do
    v <- readR16 ss
    writeR16 ss (v + 1)
    pure 2
  decss ss = CPUM $ do
    v <- readR16 ss
    writeR16 ss (v - 1)
    pure 2
  rlca = CPUM $ do
    writeR8 RegA =<< rlc =<< readR8 RegA
    pure 1
  rla = CPUM $ do
    writeR8 RegA =<< rl =<< readR8 RegA
    pure 1
  rrca = CPUM $ do
    writeR8 RegA =<< rrc =<< readR8 RegA
    pure 1
  rra = CPUM $ do
    writeR8 RegA =<< rr =<< readR8 RegA
    pure 1
  rlcr r = CPUM $ do
    writeR8 r =<< rlc =<< readR8 r
    pure 2
  rlchl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< rlc =<< readByte hl
    pure 4
  rlr r = CPUM $ do
    writeR8 r =<< rl =<< readR8 r
    pure 2
  rlhl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< rl =<< readByte hl
    pure 4
  rrcr r = CPUM $ do
    writeR8 r =<< rrc =<< readR8 r
    pure 2
  rrchl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< rrc =<< readByte hl
    pure 4
  rrr r = CPUM $ do
    writeR8 r =<< rr =<< readR8 r
    pure 2
  rrhl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< rr =<< readByte hl
    pure 4
  slar r = CPUM $ do
    writeR8 r =<< sla =<< readR8 r
    pure 2
  slahl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< sla =<< readByte hl
    pure 4
  srar r = CPUM $ do
    writeR8 r =<< sra =<< readR8 r
    pure 2
  srahl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< sra =<< readByte hl
    pure 4
  srlr r = CPUM $ do
    writeR8 r =<< srl =<< readR8 r
    pure 2
  srlhl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< srl =<< readByte hl
    pure 4
  swapr r = CPUM $ do
    writeR8 r =<< swap =<< readR8 r
    pure 2
  swaphl = CPUM $ do
    hl <- readR16 RegHL
    writeByte hl =<< swap =<< readByte hl
    pure 4
  bitr r b = CPUM $ do
    v <- readR8 r
    setFlagsMask (flagH .|. flagN .|. flagZ)
                 (flagH .|. (if v `testBit` fromIntegral b then 0 else flagZ))
    pure 2
  bithl b = CPUM $ do
    v <- readByte =<< readR16 RegHL
    setFlagsMask (flagH .|. flagN .|. flagZ)
                 (flagH .|. (if v `testBit` fromIntegral b then 0 else flagZ))
    pure 3
  setr r b = CPUM $ do
    v <- readR8 r
    writeR8 r (v `setBit` fromIntegral b)
    pure 2
  sethl b = CPUM $ do
    hl <- readR16 RegHL
    v  <- readByte hl
    writeByte hl (v `setBit` fromIntegral b)
    pure 4
  resr r b = CPUM $ do
    v <- readR8 r
    writeR8 r (v `clearBit` fromIntegral b)
    pure 2
  reshl b = CPUM $ do
    hl <- readR16 RegHL
    v  <- readByte hl
    writeByte hl (v `clearBit` fromIntegral b)
    pure 4
  jpnn nn = CPUM $ do
    writePC nn
    pure 4
  jphl = CPUM $ do
    writePC =<< readR16 RegHL
    pure 1
  jpccnn cc nn = CPUM $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        writePC nn
        pure 4
      else pure 3
  jr e = CPUM $ do
    pc <- readPC
    writePC (pc + fromIntegral e)
    pure 3
  jrcc cc e = CPUM $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        pc <- readPC
        writePC (pc + fromIntegral e)
        pure 3
      else pure 2
  call nn = CPUM $ do
    push16 =<< readPC
    writePC nn
    pure 6
  callcc cc nn = CPUM $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        push16 =<< readPC
        writePC nn
        pure 6
      else pure 3
  ret = CPUM $ do
    writePC =<< pop16
    pure 4
  reti = CPUM $ do
    writePC =<< pop16
    setIME
    pure 4
  retcc cc = CPUM $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        writePC =<< pop16
        pure 5
      else pure 2
  rst t = CPUM $ do
    push16 =<< readPC
    writePC $ 8 * fromIntegral t
    pure 4
  daa = CPUM $ do
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
    pure 1
  cpl = CPUM $ do
    a <- readR8 RegA
    writeR8 RegA (complement a)
    setFlagsMask (flagH .|. flagN) (flagH .|. flagN)
    pure 1
  nop = pure 1
  ccf = CPUM $ do
    cf <- testFlag flagCY
    setFlagsMask (flagCY .|. flagH .|. flagN) (if cf then 0 else flagCY)
    pure 1
  scf = CPUM $ do
    setFlagsMask (flagCY .|. flagH .|. flagN) flagCY
    pure 1
  di = CPUM $ do
    clearIME
    pure 1
  ei = CPUM $ do
    setIME
    pure 1
  halt = CPUM $ do
    setMode ModeHalt
    pure 1
  stop = CPUM $ do
    CPUState {..} <- asks forCPUState
    key1          <- liftIO (directReadPort portKEY1)
    if isFlagSet flagSpeedSwitch key1
      then if isFlagSet flagDoubleSpeed key1
        then liftIO $ do
          directWritePort portKEY1 0
          writeUnboxedRef cpuCycleClocks 4
        else liftIO $ do
          directWritePort portKEY1 flagDoubleSpeed
          writeUnboxedRef cpuCycleClocks 2
      else setMode ModeStop
    pure 1
  invalid b = liftIO (throwIO (InvalidInstruction b))

{-# INLINE push16 #-}
push16 :: HasCPU env => Word16 -> ReaderT env IO ()
push16 value = do
  sp <- readR16 RegSP
  let sp' = sp - 2
  writeWord sp' value
  writeR16 RegSP sp'

{-# INLINE pop16 #-}
pop16 :: HasCPU env => ReaderT env IO Word16
pop16 = do
  sp     <- readR16 RegSP
  valueL <- readByte sp
  valueH <- readByte (sp + 1)
  writeR16 RegSP (sp + 2)
  pure ((fromIntegral valueH .<<. 8) .|. fromIntegral valueL)

{-# INLINE arithOp8 #-}
arithOp8 :: HasCPU env => ArithmeticOp -> Word8 -> Bool -> ReaderT env IO ()
arithOp8 op x carry = do
  a <- readR8 RegA
  let (a', flags) = adder8 op a x carry
  writeR8 RegA a'
  setFlags flags

{-# INLINE andOp8 #-}
andOp8 :: HasCPU env => Word8 -> ReaderT env IO ()
andOp8 x = do
  a <- readR8 RegA
  let a' = a .&. x
  writeR8 RegA a'
  setFlags (flagH .|. (if a' == 0 then flagZ else 0))

{-# INLINE xorOp8 #-}
xorOp8 :: HasCPU env => Word8 -> ReaderT env IO ()
xorOp8 x = do
  a <- readR8 RegA
  let a' = a `xor` x
  writeR8 RegA a'
  setFlags (if a' == 0 then flagZ else 0)

{-# INLINE orOp8 #-}
orOp8 :: HasCPU env => Word8 -> ReaderT env IO ()
orOp8 x = do
  a <- readR8 RegA
  let a' = a .|. x
  writeR8 RegA a'
  setFlags (if a' == 0 then flagZ else 0)

{-# INLINE rlc #-}
rlc :: HasCPU env => Word8 -> ReaderT env IO Word8
rlc v = do
  setFlags (if v .&. 0x80 /= 0 then flagCY else 0)
  pure (rotateL v 1)

{-# INLINE rl #-}
rl :: HasCPU env => Word8 -> ReaderT env IO Word8
rl v = do
  let ir = rotateL v 1
  hasCY <- testFlag flagCY
  setFlags (if v .&. 0x80 /= 0 then flagCY else 0)
  pure (if hasCY then ir .|. 0x01 else ir .&. 0xFE)

{-# INLINE rrc #-}
rrc :: HasCPU env => Word8 -> ReaderT env IO Word8
rrc v = do
  setFlags (if v .&. 0x01 /= 0 then flagCY else 0)
  pure (rotateR v 1)

{-# INLINE rr #-}
rr :: HasCPU env => Word8 -> ReaderT env IO Word8
rr v = do
  let ir = rotateR v 1
  hasCY <- testFlag flagCY
  setFlags (if v .&. 0x01 /= 0 then flagCY else 0)
  pure (if hasCY then ir .|. 0x80 else ir .&. 0x7F)

{-# INLINE sla #-}
sla :: HasCPU env => Word8 -> ReaderT env IO Word8
sla v = do
  let r = v .<<. 1
  setFlags ((if v .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
  pure r

{-# INLINE sra #-}
sra :: HasCPU env => Word8 -> ReaderT env IO Word8
sra v = do
  let r = (fromIntegral v .>>. 1) :: Int8
  setFlags ((if v .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
  pure (fromIntegral r)

{-# INLINE srl #-}
srl :: HasCPU env => Word8 -> ReaderT env IO Word8
srl v = do
  let r = v .>>. 1
  setFlags ((if v .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
  pure r

{-# INLINE swap #-}
swap :: HasCPU env => Word8 -> ReaderT env IO Word8
swap v = do
  setFlags (if v == 0 then flagZ else 0)
  pure ((v .>>. 4) .|. (v .<<. 4))

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
