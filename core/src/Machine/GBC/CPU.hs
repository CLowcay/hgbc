{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.CPU
  ( RegisterFile(..)
  , Mode(..)
  , State(..)
  , Has(..)
  , M(..)
  , init
  , ports
  , getMode
  , setMode
  , getCycleClocks
  , setCycleClocks
  , getCallDepth
  , getBacktrace
  , reset
  , getRegisterFile
  , readR8
  , writeR8
  , readR16
  , readR16pp
  , writeR16
  , writeR16pp
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
  , step
  )
where

import           Control.Exception              ( throwIO )
import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Machine.GBC.CPU.Decode
import           Machine.GBC.CPU.ISA
import           Machine.GBC.CPU.Interrupts
import           Machine.GBC.Errors
import           Machine.GBC.Mode
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Registers
import           Machine.GBC.Util
import           Prelude                 hiding ( init )
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable.Mutable  as VSM
import qualified Machine.GBC.CPU.Backtrace     as Backtrace
import qualified Machine.GBC.Memory            as Memory

-- | The register file.
data RegisterFile = RegisterFile {
    regF :: !Word8
  , regA :: !Word8
  , regC :: !Word8
  , regB :: !Word8
  , regE :: !Word8
  , regD :: !Word8
  , regL :: !Word8
  , regH :: !Word8
  , regSP :: !Word16
  , regPC :: !Word16
  , regHidden :: !Word16
} deriving (Eq, Ord, Show)

offsetF, offsetA, offsetC, offsetB :: Int
offsetE, offsetD, offsetL, offsetH :: Int
offsetF = 0
offsetA = 1
offsetC = 2
offsetB = 3
offsetE = 4
offsetD = 5
offsetL = 6
offsetH = 7

offsetAF, offsetBC, offsetDE, offsetHL :: Int
offsetPC, offsetSP, offsetHidden :: Int
offsetAF = 0
offsetBC = 1
offsetDE = 2
offsetHL = 3
offsetSP = 4
offsetPC = 5
offsetHidden = 6

instance Storable RegisterFile where
  sizeOf _ = 14
  alignment _ = 2
  peek ptr = do
    regA      <- peekElemOff (castPtr ptr) offsetA
    regB      <- peekElemOff (castPtr ptr) offsetB
    regC      <- peekElemOff (castPtr ptr) offsetC
    regD      <- peekElemOff (castPtr ptr) offsetD
    regE      <- peekElemOff (castPtr ptr) offsetE
    regF      <- peekElemOff (castPtr ptr) offsetF
    regH      <- peekElemOff (castPtr ptr) offsetH
    regL      <- peekElemOff (castPtr ptr) offsetL
    regSP     <- peekElemOff (castPtr ptr) offsetSP
    regPC     <- peekElemOff (castPtr ptr) offsetPC
    regHidden <- peekElemOff (castPtr ptr) offsetHidden
    pure RegisterFile { .. }
  poke ptr RegisterFile {..} = do
    pokeElemOff (castPtr ptr) offsetA      regA
    pokeElemOff (castPtr ptr) offsetB      regB
    pokeElemOff (castPtr ptr) offsetC      regC
    pokeElemOff (castPtr ptr) offsetD      regD
    pokeElemOff (castPtr ptr) offsetE      regE
    pokeElemOff (castPtr ptr) offsetF      regF
    pokeElemOff (castPtr ptr) offsetH      regH
    pokeElemOff (castPtr ptr) offsetL      regL
    pokeElemOff (castPtr ptr) offsetSP     regSP
    pokeElemOff (castPtr ptr) offsetPC     regPC
    pokeElemOff (castPtr ptr) offsetHidden regHidden

-- | The current CPU mode.
data Mode = ModeHalt | ModeStop | ModeNormal deriving (Eq, Ord, Show, Bounded, Enum)

-- | The internal CPU state.
data State = State {
    cpuType     :: !EmulatorMode
  , busCatchup  :: BusCatchupFunction
  , registers   :: !(VSM.IOVector RegisterFile)
  , portIF      :: !(Port Word8)
  , portIE      :: !(Port Word8)
  , portKEY1    :: !(Port Word8)
  , cpuMode     :: !(IORef Mode)
  , cycleClocks :: !(UnboxedRef Int)
  , callDepth   :: !(UnboxedRef Int)
  , backtrace   :: !Backtrace.Backtrace
  , haltBug     :: !(IORef Bool)
}

class Memory.Has env => Has env where
  forState :: env -> State

type BusCatchupFunction = Int -> Int -> IO ()

-- | Initialize a new CPU.
init :: Port Word8 -> Port Word8 -> EmulatorMode -> BusCatchupFunction -> IO State
init portIF portIE cpuType busCatchup = do
  registers   <- VSM.new 1
  portKEY1    <- newPort 0x00 0x01 alwaysUpdate
  cpuMode     <- newIORef ModeNormal
  cycleClocks <- newUnboxedRef 4
  callDepth   <- newUnboxedRef 0
  backtrace   <- Backtrace.new 8
  haltBug     <- newIORef False
  pure State { .. }

ports :: State -> [(Word16, Port Word8)]
ports State {..} = [(KEY1, portKEY1)]

-- | Get the current cpu mode.
{-# INLINABLE getMode #-}
getMode :: Has env => ReaderT env IO Mode
getMode = do
  State {..} <- asks forState
  liftIO $ readIORef cpuMode

-- | Get the CPU mode.
{-# INLINABLE setMode #-}
setMode :: Has env => Mode -> ReaderT env IO ()
setMode mode = do
  State {..} <- asks forState
  liftIO $ writeIORef cpuMode mode

-- | Get the values of all the registers.
{-# INLINABLE getRegisterFile #-}
getRegisterFile :: Has env => ReaderT env IO RegisterFile
getRegisterFile = do
  State {..} <- asks forState
  liftIO $ VSM.unsafeRead registers 0

-- | Read data from the register file.
{-# INLINE readRegister #-}
readRegister :: (Has env, Storable a) => Int -> ReaderT env IO a
readRegister offset = do
  State {..} <- asks forState
  liftIO $ VSM.unsafeRead (VSM.unsafeCast registers) offset

-- | Write data to the register file.
{-# INLINE writeRegister #-}
writeRegister :: (Has env, Storable a) => Int -> a -> ReaderT env IO ()
writeRegister offset value = do
  State {..} <- asks forState
  liftIO $ VSM.unsafeWrite (VSM.unsafeCast registers) offset value

-- | Read a single register.
{-# INLINABLE readR8 #-}
readR8 :: Has env => Register8 -> ReaderT env IO Word8
readR8 = readRegister . offsetR8

-- | Write a single register.
{-# INLINABLE writeR8 #-}
writeR8 :: Has env => Register8 -> Word8 -> ReaderT env IO ()
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
readR16 :: Has env => Register16 -> ReaderT env IO Word16
readR16 = readRegister . offsetR16

-- | Write a 16-bit register.
{-# INLINABLE writeR16 #-}
writeR16 :: Has env => Register16 -> Word16 -> ReaderT env IO ()
writeR16 register = writeRegister $ offsetR16 register

-- | Get the offset in the register file of a register pair.
offsetR16 :: Register16 -> Int
offsetR16 RegBC = offsetBC
offsetR16 RegDE = offsetDE
offsetR16 RegHL = offsetHL
offsetR16 RegSP = offsetSP

-- | Read a 16-bit register.
{-# INLINABLE readR16pp #-}
readR16pp :: Has env => RegisterPushPop -> ReaderT env IO Word16
readR16pp register = readRegister (offsetR16pp register)

-- | Write a 16-bit register.
{-# INLINABLE writeR16pp #-}
writeR16pp :: Has env => RegisterPushPop -> Word16 -> ReaderT env IO ()
writeR16pp PushPopAF v = writeRegister offsetF (v .&. 0xFFF0)
writeR16pp register  v = writeRegister (offsetR16pp register) v

-- | Get the offset in the register file of a register pair.
offsetR16pp :: RegisterPushPop -> Int
offsetR16pp PushPopBC = offsetBC
offsetR16pp PushPopDE = offsetDE
offsetR16pp PushPopHL = offsetHL
offsetR16pp PushPopAF = offsetAF

-- | Read the PC register.
{-# INLINABLE readPC #-}
readPC :: Has env => ReaderT env IO Word16
readPC = readRegister offsetPC

-- | Write the PC register.
{-# INLINABLE writePC #-}
writePC :: Has env => Word16 -> ReaderT env IO ()
writePC = writeRegister offsetPC

type Flag = Word8
flagZ, flagN, flagH, flagCY :: Flag
flagZ = 0x80
flagN = 0x40
flagH = 0x20
flagCY = 0x10

allExceptCY :: Word8
allExceptCY = flagZ .|. flagN .|. flagH

allExceptZ :: Word8
allExceptZ = flagH .|. flagN .|. flagCY

allExceptN :: Word8
allExceptN = flagH .|. flagCY .|. flagZ

-- Master interrupt enable flag
{-# INLINABLE flagIME #-}
flagIME :: Word16
flagIME = 0x0100

{-# INLINABLE flagSetIME #-}
flagSetIME :: Word16
flagSetIME = 0x0200

-- | Check if a flag is set.
{-# INLINE testFlag #-}
testFlag :: Has env => Flag -> ReaderT env IO Bool
testFlag flag = do
  f <- readRegister offsetF
  pure (f .&. flag /= 0)

-- | Check if a condition code is true.
{-# INLINE testCondition #-}
testCondition :: Has env => ConditionCode -> ReaderT env IO Bool
testCondition CondNZ = not <$> testFlag flagZ
testCondition CondZ  = testFlag flagZ
testCondition CondNC = not <$> testFlag flagCY
testCondition CondC  = testFlag flagCY

-- | Read the F register.
{-# INLINE readF #-}
readF :: Has env => ReaderT env IO Word8
readF = readRegister offsetF

-- | Write the F register.
{-# INLINE writeF #-}
writeF :: Has env => Word8 -> ReaderT env IO ()
writeF = writeRegister offsetF

-- | Set all the flags.
{-# INLINE setFlags #-}
setFlags :: Has env => Word8 -> ReaderT env IO ()
setFlags = writeF

-- | Set some flags.
{-# INLINE setFlagsMask #-}
setFlagsMask
  :: Has env
  => Word8 -- ^ bitmask containing flags to set.
  -> Word8 -- ^ new flags values.
  -> ReaderT env IO ()
setFlagsMask mask flags = do
  oldFlags <- readF
  writeF ((oldFlags .&. complement mask) .|. (flags .&. mask))

-- | Set the master interrupt flag.
{-# INLINE setIME #-}
setIME :: Has env => ReaderT env IO ()
setIME = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .|. flagIME)

-- | Clear the master interrupt flag.
{-# INLINE clearIME #-}
clearIME :: Has env => ReaderT env IO ()
clearIME = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .&. complement flagIME)

{-# INLINE setIMENext #-}
setIMENext :: Has env => ReaderT env IO ()
setIMENext = do
  ime <- readRegister offsetHidden
  writeRegister offsetHidden (ime .|. flagSetIME)

{-# INLINE updateIME #-}
updateIME :: Has env => ReaderT env IO ()
updateIME = do
  ime <- readRegister offsetHidden
  when (ime .&. flagSetIME /= 0)
    $ writeRegister offsetHidden ((ime .|. flagIME) .&. complement flagSetIME)

-- | Check the status of the interrupt flag.
{-# INLINE testIME #-}
testIME :: Has env => ReaderT env IO Bool
testIME = do
  ime <- readRegister offsetHidden
  pure (ime .&. flagIME /= 0)

{-# INLINE runBusCatchup #-}
runBusCatchup :: Has env => Int -> ReaderT env IO ()
runBusCatchup cycles = do
  State {..} <- asks forState
  clocks     <- getCycleClocks
  liftIO (busCatchup cycles clocks)

-- | Reset the CPU.
{-# INLINABLE reset #-}
reset :: Has env => ReaderT env IO ()
reset = do
  State {..} <- asks forState

  writeR8 RegA 0
  writeF 0
  writeR8 RegB 0
  writeR8 RegC 0
  writeR8 RegD 0
  writeR8 RegE 0
  writeR8 RegH 0
  writeR8 RegL 0
  writeR16 RegSP 0
  writeRegister offsetHidden (0 :: Word8)
  setIME
  writePC 0

  liftIO $ do
    directWritePort portKEY1 0
    writeIORef cpuMode ModeNormal
    writeUnboxedRef cycleClocks 4
    writeIORef haltBug False
    writeUnboxedRef callDepth 0
    Backtrace.reset backtrace

  Memory.writeByte P1 0xFF
  Memory.writeByte TIMA 0
  Memory.writeByte TMA 0
  Memory.writeByte TAC 0
  Memory.writeByte NR10 0
  Memory.writeByte NR11 0
  Memory.writeByte NR12 0
  Memory.writeByte NR13 0
  Memory.writeByte NR14 0
  Memory.writeByte NR21 0
  Memory.writeByte NR22 0
  Memory.writeByte NR23 0
  Memory.writeByte NR24 0
  Memory.writeByte NR30 0
  Memory.writeByte NR31 0
  Memory.writeByte NR32 0
  Memory.writeByte NR33 0
  Memory.writeByte NR34 0
  Memory.writeByte NR41 0
  Memory.writeByte NR42 0
  Memory.writeByte NR43 0
  Memory.writeByte NR44 0
  Memory.writeByte NR50 0
  Memory.writeByte NR51 0
  Memory.writeByte NR52 0
  Memory.writeByte LCDC 0
  Memory.writeByte SCY 0
  Memory.writeByte SCX 0
  Memory.writeByte LYC 0
  Memory.writeByte BGP 0
  Memory.writeByte OBP0 0
  Memory.writeByte OBP1 0
  Memory.writeByte WY 0
  Memory.writeByte WX 0
  Memory.writeByte IE 0
  Memory.writeByte IF 0

  -- Wave memory
  Memory.writeByte 0xFF30 0x00
  Memory.writeByte 0xFF31 0xFF
  Memory.writeByte 0xFF32 0x00
  Memory.writeByte 0xFF33 0xFF
  Memory.writeByte 0xFF34 0x00
  Memory.writeByte 0xFF35 0xFF
  Memory.writeByte 0xFF36 0x00
  Memory.writeByte 0xFF37 0xFF
  Memory.writeByte 0xFF38 0x00
  Memory.writeByte 0xFF39 0xFF
  Memory.writeByte 0xFF3A 0x00
  Memory.writeByte 0xFF3B 0xFF
  Memory.writeByte 0xFF3C 0x00
  Memory.writeByte 0xFF3D 0xFF
  Memory.writeByte 0xFF3E 0x00
  Memory.writeByte 0xFF3F 0xFF

  Memory.resetAndBoot $ do
    -- Set all registers and ports to their post-boot values.
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

    Memory.writeByte NR11 0xBF
    Memory.writeByte NR12 0xF3
    Memory.writeByte NR50 0x77
    Memory.writeByte NR51 0xF3
    Memory.writeByte NR52 0xF1
    Memory.writeByte LCDC 0x91
    Memory.writeByte BGP 0xFC
    Memory.writeByte OBP0 0xFF
    Memory.writeByte OBP1 0xFF

-- | Perform an arithmetic operation and adjust the flags.
{-# INLINE adder8 #-}
adder8 :: Word8 -> Word8 -> Word16 -> Word16 -> (Word8, Word8)
adder8 a1 a2 wa2' carry =
  let
    wa1     = fromIntegral a1
    wa2     = fromIntegral a2
    wr      = wa1 + wa2' + carry
    r       = fromIntegral wr
    carryH  = (wa1 .&. 0x0010) `xor` (wa2 .&. 0x0010) /= (wr .&. 0x0010)
    carryCY = (wr .&. 0x0100) /= 0
    flags =
      (if r == 0 then flagZ else 0)
        .|. (if carryH then flagH else 0)
        .|. (if carryCY then flagCY else 0)
  in
    (r, flags)

getCarry :: Has env => ReaderT env IO Word16
getCarry = do
  f <- readF
  pure ((fromIntegral f .>>. 4) .&. 1)

negative1 :: Word8
negative1 = negate 1

-- | Perform an increment operation and adjust the flags.
{-# INLINE inc8 #-}
inc8 :: Word8 -> Word8 -> (Word8, Word8)
inc8 value x =
  let r      = value + x
      carryH = (value .&. 0x10) /= (r .&. 0x010)
      flags  = (if r == 0 then flagZ else 0) .|. (if carryH then flagH else 0)
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
{-# INLINABLE step #-}
step :: Has env => ReaderT env IO Int
step = do
  State {..} <- asks forState

  -- Check if we have an interrupt
  interrupts <- liftIO $ pendingEnabledInterrupts portIF portIE
  ime        <- testIME
  updateIME

  -- Deal with HALT mode
  mode <- liftIO (readIORef cpuMode)
  case mode of
    ModeNormal -> if interrupts /= 0 && ime then handleInterrupt interrupts else executeInstruction
    ModeHalt   -> if interrupts /= 0
      then do
        liftIO (writeIORef cpuMode ModeNormal)
        if ime then handleInterrupt interrupts else executeInstruction
      else pure 4
    ModeStop -> if interrupts /= 0
      then do
        liftIO (writeIORef cpuMode ModeNormal)
        if ime then handleInterrupt interrupts else executeInstruction
      else pure 4

 where
  handleInterrupt interrupts = do
    -- Handle an interrupt
    let nextInterrupt = getNextInterrupt interrupts
    doCall (interruptVector nextInterrupt)
    clearIME
    State {..} <- asks forState
    liftIO $ clearInterrupt portIF nextInterrupt
    pure 5

{-# INLINE getCycleClocks #-}
getCycleClocks :: Has env => ReaderT env IO Int
getCycleClocks = do
  State {..} <- asks forState
  liftIO (readUnboxedRef cycleClocks)

{-# INLINE setCycleClocks #-}
setCycleClocks :: Has env => Int -> ReaderT env IO ()
setCycleClocks clocks = do
  State {..} <- asks forState
  liftIO (writeUnboxedRef cycleClocks clocks)

{-# INLINE getCallDepth #-}
getCallDepth :: Has env => ReaderT env IO Int
getCallDepth = do
  State {..} <- asks forState
  liftIO (readUnboxedRef callDepth)

getBacktrace :: Has env => ReaderT env IO [(Word16, Word16)]
getBacktrace = do
  State {..} <- asks forState
  liftIO (Backtrace.toList backtrace)

{-# INLINE callStackPushed #-}
callStackPushed :: Has env => Word16 -> ReaderT env IO ()
callStackPushed offset = do
  State {..} <- asks forState
  bank       <- Memory.getBank offset
  liftIO $ do
    d <- readUnboxedRef callDepth
    writeUnboxedRef callDepth (d + 1)
    Backtrace.push backtrace bank offset

{-# INLINE callStackPopped #-}
callStackPopped :: Has env => ReaderT env IO ()
callStackPopped = do
  State {..} <- asks forState
  liftIO $ do
    d <- readUnboxedRef callDepth
    writeUnboxedRef callDepth (d - 1)
    Backtrace.pop backtrace

{-# SPECIALIZE table0 :: Has env => V.Vector (M env Int) #-}
{-# SPECIALIZE table1 :: Has env => V.Vector (M env Int) #-}
{-# SPECIALIZE fetchAndExecute :: Has env => M env Int #-}
{-# INLINABLE executeInstruction #-}
executeInstruction :: Has env => ReaderT env IO Int
executeInstruction = run fetchAndExecute

newtype M env a = M {run :: ReaderT env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Has env => MonadFetch (M env) where
  nextByte = M $ do
    pc         <- readPC
    State {..} <- asks forState
    doHaltBug  <- liftIO $ readIORef haltBug
    if doHaltBug then liftIO $ writeIORef haltBug False else writePC (pc + 1)
    Memory.readByte pc

instance Has env => MonadGMBZ80 (M env) where
  type ExecuteResult (M env) = Int
  ldrr r r' = M $ do
    writeR8 r =<< readR8 r'
    pure 1
  ldrn r n = M $ do
    writeR8 r n
    pure 2
  ldrHL r = M $ do
    hl <- readR16 RegHL
    writeR8 r =<< Memory.readByte hl
    pure 2
  ldHLr r = M $ do
    hl <- readR16 RegHL
    Memory.writeByte hl =<< readR8 r
    pure 2
  ldHLn n = M $ do
    runBusCatchup 1
    hl <- readR16 RegHL
    Memory.writeByte hl n
    pure 2
  ldaBC = M $ do
    bc <- readR16 RegBC
    writeR8 RegA =<< Memory.readByte bc
    pure 2
  ldaDE = M $ do
    de <- readR16 RegDE
    writeR8 RegA =<< Memory.readByte de
    pure 2
  ldaC = M $ do
    c <- readR8 RegC
    writeR8 RegA =<< Memory.readByte (0xFF00 + fromIntegral c)
    pure 2
  ldCa = M $ do
    c <- readR8 RegC
    Memory.writeByte (0xFF00 + fromIntegral c) =<< readR8 RegA
    pure 2
  ldan n = M $ do
    runBusCatchup 1
    writeR8 RegA =<< Memory.readByte (0xFF00 + fromIntegral n)
    pure 2
  ldna n = M $ do
    runBusCatchup 1
    Memory.writeByte (0xFF00 + fromIntegral n) =<< readR8 RegA
    pure 2
  ldann nn = M $ do
    runBusCatchup 2
    writeR8 RegA =<< Memory.readByte nn
    pure 2
  ldnna nn = M $ do
    runBusCatchup 2
    Memory.writeByte nn =<< readR8 RegA
    pure 2
  ldaHLI = M $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl + 1)
    writeR8 RegA =<< Memory.readByte hl
    pure 2
  ldaHLD = M $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl - 1)
    writeR8 RegA =<< Memory.readByte hl
    pure 2
  ldBCa = M $ do
    bc <- readR16 RegBC
    Memory.writeByte bc =<< readR8 RegA
    pure 2
  ldDEa = M $ do
    de <- readR16 RegDE
    Memory.writeByte de =<< readR8 RegA
    pure 2
  ldHLIa = M $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl + 1)
    Memory.writeByte hl =<< readR8 RegA
    pure 2
  ldHLDa = M $ do
    hl <- readR16 RegHL
    writeR16 RegHL (hl - 1)
    Memory.writeByte hl =<< readR8 RegA
    pure 2
  ldddnn dd nn = M $ do
    writeR16 dd nn
    pure 3
  ldSPHL = M $ do
    writeR16 RegSP =<< readR16 RegHL
    pure 2
  push qq = M $ do
    push16 =<< readR16pp qq
    pure 4
  pop qq = M $ do
    writeR16pp qq =<< pop16
    pure 3
  ldhl i = M $ do
    sp <- fromIntegral <$> readR16 RegSP
    let wi      = fromIntegral i :: Int32
    let wr      = sp + wi
    let carryH = (sp .&. 0x00000010) `xor` (wi .&. 0x00000010) /= (wr .&. 0x00000010)
    let carryCY = (sp .&. 0x00000100) `xor` (wi .&. 0x00000100) /= (wr .&. 0x00000100)
    writeR16 RegHL (fromIntegral wr)
    setFlags ((if carryCY then flagCY else 0) .|. (if carryH then flagH else 0))
    pure 3
  ldnnSP nn = M $ do
    Memory.writeWord nn =<< readR16 RegSP
    pure 5
  addr r = M $ do
    v <- readR8 r
    add8 v 0
    pure 1
  addn n = M $ do
    add8 n 0
    pure 2
  addhl = M $ do
    v <- Memory.readByte =<< readR16 RegHL
    add8 v 0
    pure 2
  adcr r = M $ do
    v     <- readR8 r
    carry <- getCarry
    add8 v carry
    pure 1
  adcn n = M $ do
    carry <- getCarry
    add8 n carry
    pure 2
  adchl = M $ do
    v     <- Memory.readByte =<< readR16 RegHL
    carry <- getCarry
    add8 v carry
    pure 2
  subr r = M $ do
    v <- readR8 r
    sub8 v 0
    pure 1
  subn n = M $ do
    sub8 n 0
    pure 2
  subhl = M $ do
    v <- Memory.readByte =<< readR16 RegHL
    sub8 v 0
    pure 2
  sbcr r = M $ do
    v     <- readR8 r
    carry <- getCarry
    sub8 v (negate carry)
    pure 1
  sbcn n = M $ do
    carry <- getCarry
    sub8 n (negate carry)
    pure 2
  sbchl = M $ do
    v     <- Memory.readByte =<< readR16 RegHL
    carry <- getCarry
    sub8 v (negate carry)
    pure 2
  andr r = M $ do
    andOp8 =<< readR8 r
    pure 1
  andn n = M $ do
    andOp8 n
    pure 2
  andhl = M $ do
    andOp8 =<< Memory.readByte =<< readR16 RegHL
    pure 2
  orr r = M $ do
    orOp8 =<< readR8 r
    pure 1
  orn n = M $ do
    orOp8 n
    pure 2
  orhl = M $ do
    orOp8 =<< Memory.readByte =<< readR16 RegHL
    pure 2
  xorr r = M $ do
    xorOp8 =<< readR8 r
    pure 1
  xorn n = M $ do
    xorOp8 n
    pure 2
  xorhl = M $ do
    xorOp8 =<< Memory.readByte =<< readR16 RegHL
    pure 2
  cpr r = M $ do
    a <- readR8 RegA
    v <- readR8 r
    let (_, flags) = adder8 a v (negate (fromIntegral v)) 0
    setFlags (flagN .|. flags)
    pure 1
  cpn n = M $ do
    a <- readR8 RegA
    let (_, flags) = adder8 a n (negate (fromIntegral n)) 0
    setFlags (flagN .|. flags)
    pure 2
  cphl = M $ do
    a <- readR8 RegA
    v <- Memory.readByte =<< readR16 RegHL
    let (_, flags) = adder8 a v (negate (fromIntegral v)) 0
    setFlags (flagN .|. flags)
    pure 2
  incr r = M $ do
    v <- readR8 r
    let (v', flags) = inc8 v 1
    writeR8 r v'
    setFlagsMask allExceptCY flags
    pure 1
  inchl = M $ do
    hl <- readR16 RegHL
    v  <- Memory.readByte hl
    runBusCatchup 1
    let (v', flags) = inc8 v 1
    setFlagsMask allExceptCY flags
    Memory.writeByte hl v'
    pure 2
  decr r = M $ do
    v <- readR8 r
    let (v', flags) = inc8 v negative1
    writeR8 r v'
    setFlagsMask allExceptCY (flags .|. flagN)
    pure 1
  dechl = M $ do
    hl <- readR16 RegHL
    v  <- Memory.readByte hl
    runBusCatchup 1
    let (v', flags) = inc8 v negative1
    setFlagsMask allExceptCY (flags .|. flagN)
    Memory.writeByte hl v'
    pure 2
  addhlss ss = M $ do
    hl <- readR16 RegHL
    v  <- readR16 ss
    let hl'     = fromIntegral hl
    let v'      = fromIntegral v
    let wr      = hl' + v' :: Word32
    let carryH = (hl' .&. 0x00001000) `xor` (v' .&. 0x00001000) /= (wr .&. 0x00001000)
    let carryCY = (wr .&. 0x00010000) /= 0
    writeR16 RegHL (fromIntegral wr)
    setFlagsMask allExceptZ ((if carryH then flagH else 0) .|. (if carryCY then flagCY else 0))
    pure 2
  addSP e = M $ do
    sp <- readR16 RegSP
    let sp'     = fromIntegral sp
    let e'      = fromIntegral e
    let wr      = e' + sp' :: Int32
    let carryH = (sp' .&. 0x00000010) `xor` (e' .&. 0x00000010) /= (wr .&. 0x00000010)
    let carryCY = (sp' .&. 0x00000100) `xor` (e' .&. 0x00000100) /= (wr .&. 0x00000100)
    writeR16 RegSP (fromIntegral (wr .&. 0xFFFF))
    setFlags ((if carryH then flagH else 0) .|. (if carryCY then flagCY else 0))
    pure 4
  incss ss = M $ do
    v <- readR16 ss
    writeR16 ss (v + 1)
    pure 2
  decss ss = M $ do
    v <- readR16 ss
    writeR16 ss (v - 1)
    pure 2
  rlca = M $ do
    v <- readR8 RegA
    setFlags (if v .&. 0x80 /= 0 then flagCY else 0)
    writeR8 RegA (rotateL v 1)
    pure 1
  rla = M $ do
    v <- readR8 RegA
    let ir = rotateL v 1
    hasCY <- testFlag flagCY
    setFlags (if v .&. 0x80 /= 0 then flagCY else 0)
    writeR8 RegA (if hasCY then ir .|. 0x01 else ir .&. 0xFE)
    pure 1
  rrca = M $ do
    v <- readR8 RegA
    setFlags (if v .&. 0x01 /= 0 then flagCY else 0)
    writeR8 RegA (rotateR v 1)
    pure 1
  rra = M $ do
    v <- readR8 RegA
    let ir = rotateR v 1
    hasCY <- testFlag flagCY
    setFlags (if v .&. 0x01 /= 0 then flagCY else 0)
    writeR8 RegA (if hasCY then ir .|. 0x80 else ir .&. 0x7F)
    pure 1
  rlcr r = M $ do
    writeR8 r =<< rlc =<< readR8 r
    pure 2
  rlchl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< rlc v
    pure 2
  rlr r = M $ do
    writeR8 r =<< rl =<< readR8 r
    pure 2
  rlhl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< rl v
    pure 2
  rrcr r = M $ do
    writeR8 r =<< rrc =<< readR8 r
    pure 2
  rrchl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< rrc v
    pure 2
  rrr r = M $ do
    writeR8 r =<< rr =<< readR8 r
    pure 2
  rrhl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< rr v
    pure 2
  slar r = M $ do
    writeR8 r =<< sla =<< readR8 r
    pure 2
  slahl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< sla v
    pure 2
  srar r = M $ do
    writeR8 r =<< sra =<< readR8 r
    pure 2
  srahl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< sra v
    pure 2
  srlr r = M $ do
    writeR8 r =<< srl =<< readR8 r
    pure 2
  srlhl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< srl v
    pure 2
  swapr r = M $ do
    writeR8 r =<< swap =<< readR8 r
    pure 2
  swaphl = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl =<< swap v
    pure 2
  bitr r b = M $ do
    v <- readR8 r
    setFlagsMask allExceptCY (flagH .|. (if v `testBit` fromIntegral b then 0 else flagZ))
    pure 2
  bithl b = M $ do
    runBusCatchup 1
    v <- Memory.readByte =<< readR16 RegHL
    setFlagsMask allExceptCY (flagH .|. (if v `testBit` fromIntegral b then 0 else flagZ))
    pure 2
  setr r b = M $ do
    v <- readR8 r
    writeR8 r (v `setBit` fromIntegral b)
    pure 2
  sethl b = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl (v `setBit` fromIntegral b)
    pure 2
  resr r b = M $ do
    v <- readR8 r
    writeR8 r (v `clearBit` fromIntegral b)
    pure 2
  reshl b = M $ do
    hl <- readR16 RegHL
    runBusCatchup 1
    v <- Memory.readByte hl
    runBusCatchup 1
    Memory.writeByte hl (v `clearBit` fromIntegral b)
    pure 2
  jpnn nn = M $ do
    writePC nn
    pure 4
  jphl = M $ do
    writePC =<< readR16 RegHL
    pure 1
  jpccnn cc nn = M $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        writePC nn
        pure 4
      else pure 3
  jr e = M $ do
    pc <- readPC
    writePC (pc + fromIntegral e)
    pure 3
  jrcc cc e = M $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        pc <- readPC
        writePC (pc + fromIntegral e)
        pure 3
      else pure 2
  call nn = 6 <$ M (doCall nn)
  callcc cc nn = M $ do
    shouldJump <- testCondition cc
    if shouldJump then 6 <$ doCall nn else pure 3
  ret = M $ do
    callStackPopped
    writePC =<< pop16
    pure 4
  reti = M $ do
    callStackPopped
    writePC =<< pop16
    setIME
    pure 4
  retcc cc = M $ do
    shouldJump <- testCondition cc
    if shouldJump
      then do
        callStackPopped
        writePC =<< pop16
        pure 5
      else pure 2
  rst t = 4 <$ M (doCall (8 * fromIntegral t))
  daa = M $ do
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
      allExceptN
      ((if isCy || rWide .&. 0x100 == 0x100 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
    pure 1
  cpl = M $ do
    a <- readR8 RegA
    writeR8 RegA (complement a)
    let flagHN = flagH .|. flagN in setFlagsMask flagHN flagHN
    pure 1
  nop = pure 1
  ccf = M $ do
    cf <- testFlag flagCY
    setFlagsMask allExceptZ (if cf then 0 else flagCY)
    pure 1
  scf = M $ do
    setFlagsMask allExceptZ flagCY
    pure 1
  di = M $ do
    clearIME
    pure 1
  ei = M $ do
    setIMENext
    pure 1
  halt = M $ do
    ime <- testIME
    if not ime
      then do
        State {..} <- asks forState
        interrupts <- liftIO $ pendingEnabledInterrupts portIF portIE
        if interrupts == 0 then setMode ModeHalt else liftIO $ writeIORef haltBug True
      else setMode ModeHalt
    pure 1
  stop = M $ do
    State {..} <- asks forState
    key1       <- liftIO (directReadPort portKEY1)
    if isFlagSet flagSpeedSwitch key1
      then if isFlagSet flagDoubleSpeed key1
        then liftIO $ do
          directWritePort portKEY1 0
          writeUnboxedRef cycleClocks 4
        else liftIO $ do
          directWritePort portKEY1 flagDoubleSpeed
          writeUnboxedRef cycleClocks 2
      else setMode ModeStop
    pure 1
  invalid b = liftIO (throwIO (InvalidInstruction b))

{-# INLINE doCall #-}
doCall :: Has env => Word16 -> ReaderT env IO ()
doCall nn = do
  callStackPushed nn
  push16 =<< readPC
  writePC nn

{-# INLINE push16 #-}
push16 :: Has env => Word16 -> ReaderT env IO ()
push16 value = do
  sp <- readR16 RegSP
  let sp' = sp - 2
  Memory.writeWord sp' value
  writeR16 RegSP sp'

{-# INLINE pop16 #-}
pop16 :: Has env => ReaderT env IO Word16
pop16 = do
  sp     <- readR16 RegSP
  valueL <- Memory.readByte sp
  valueH <- Memory.readByte (sp + 1)
  writeR16 RegSP (sp + 2)
  pure ((fromIntegral valueH .<<. 8) .|. fromIntegral valueL)

{-# INLINE add8 #-}
add8 :: Has env => Word8 -> Word16 -> ReaderT env IO ()
add8 x carry = do
  a <- readR8 RegA
  let (a', flags) = adder8 a x (fromIntegral x) carry
  writeR8 RegA a'
  setFlags flags

{-# INLINE sub8 #-}
sub8 :: Has env => Word8 -> Word16 -> ReaderT env IO ()
sub8 x carry = do
  a <- readR8 RegA
  let (a', flags) = adder8 a x (negate (fromIntegral x)) carry
  writeR8 RegA a'
  setFlags (flags .|. flagN)

{-# INLINE andOp8 #-}
andOp8 :: Has env => Word8 -> ReaderT env IO ()
andOp8 x = do
  a <- readR8 RegA
  let a' = a .&. x
  writeR8 RegA a'
  setFlags (flagH .|. (if a' == 0 then flagZ else 0))

{-# INLINE xorOp8 #-}
xorOp8 :: Has env => Word8 -> ReaderT env IO ()
xorOp8 x = do
  a <- readR8 RegA
  let a' = a `xor` x
  writeR8 RegA a'
  setFlags (if a' == 0 then flagZ else 0)

{-# INLINE orOp8 #-}
orOp8 :: Has env => Word8 -> ReaderT env IO ()
orOp8 x = do
  a <- readR8 RegA
  let a' = a .|. x
  writeR8 RegA a'
  setFlags (if a' == 0 then flagZ else 0)

{-# INLINE rlc #-}
rlc :: Has env => Word8 -> ReaderT env IO Word8
rlc v = do
  setFlags (if v == 0 then flagZ else if v .&. 0x80 /= 0 then flagCY else 0)
  pure (rotateL v 1)

{-# INLINE rl #-}
rl :: Has env => Word8 -> ReaderT env IO Word8
rl v = do
  let ir = rotateL v 1
  hasCY <- testFlag flagCY
  let r = if hasCY then ir .|. 0x01 else ir .&. 0xFE
  setFlags ((if r == 0 then flagZ else 0) .|. (if v .&. 0x80 /= 0 then flagCY else 0))
  pure r

{-# INLINE rrc #-}
rrc :: Has env => Word8 -> ReaderT env IO Word8
rrc v = do
  setFlags (if v == 0 then flagZ else if v .&. 0x01 /= 0 then flagCY else 0)
  pure (rotateR v 1)

{-# INLINE rr #-}
rr :: Has env => Word8 -> ReaderT env IO Word8
rr v = do
  let ir = rotateR v 1
  hasCY <- testFlag flagCY
  let r = if hasCY then ir .|. 0x80 else ir .&. 0x7F
  setFlags ((if r == 0 then flagZ else 0) .|. (if v .&. 0x01 /= 0 then flagCY else 0))
  pure r

{-# INLINE sla #-}
sla :: Has env => Word8 -> ReaderT env IO Word8
sla v = do
  let r = v .<<. 1
  setFlags ((if v .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
  pure r

{-# INLINE sra #-}
sra :: Has env => Word8 -> ReaderT env IO Word8
sra v = do
  let r = (fromIntegral v .>>. 1) :: Int8
  setFlags ((if v .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
  pure (fromIntegral r)

{-# INLINE srl #-}
srl :: Has env => Word8 -> ReaderT env IO Word8
srl v = do
  let r = v .>>. 1
  setFlags ((if v .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0))
  pure r

{-# INLINE swap #-}
swap :: Has env => Word8 -> ReaderT env IO Word8
swap v = do
  setFlags (if v == 0 then flagZ else 0)
  pure ((v .>>. 4) .|. (v .<<. 4))
