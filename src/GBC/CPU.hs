{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.CPU where

import           Data.Word
import           Foreign.Storable
import           Data.Bits
import           Foreign.ForeignPtr
import           GBC.Memory
import           GBC.ISA
import           GBC.Decode
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Int

-- | The register file.
data RegisterFile = RegisterFile {
    regA :: Word8
  , regB :: Word8
  , regC :: Word8
  , regD :: Word8
  , regE :: Word8
  , regF :: Word8
  , regH :: Word8
  , regL :: Word8
  , regSP :: Word16
  , regPC :: Word16
} deriving (Eq, Ord, Show)

offsetF, offsetA, offsetC, offsetB :: Int
offsetE, offsetD, offsetL, offsetH, offsetPC, offsetSP :: Int
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

instance Storable RegisterFile where
  sizeOf _ = 12
  alignment _ = 2
  peek ptr = do
    regA  <- peekByteOff ptr offsetA
    regB  <- peekByteOff ptr offsetB
    regC  <- peekByteOff ptr offsetC
    regD  <- peekByteOff ptr offsetD
    regE  <- peekByteOff ptr offsetE
    regF  <- peekByteOff ptr offsetF
    regH  <- peekByteOff ptr offsetH
    regL  <- peekByteOff ptr offsetL
    regSP <- peekByteOff ptr offsetSP
    regPC <- peekByteOff ptr offsetPC
    pure RegisterFile { .. }
  poke ptr RegisterFile {..} = do
    pokeByteOff ptr offsetA  regA
    pokeByteOff ptr offsetB  regB
    pokeByteOff ptr offsetC  regC
    pokeByteOff ptr offsetD  regD
    pokeByteOff ptr offsetE  regE
    pokeByteOff ptr offsetF  regF
    pokeByteOff ptr offsetH  regH
    pokeByteOff ptr offsetL  regL
    pokeByteOff ptr offsetSP regSP
    pokeByteOff ptr offsetPC regPC

-- | Information for the debugger.
data DebugInfo = DebugInfo {
    readAddress :: [Word16]
  , writeAddress :: [Word16]
} deriving (Eq, Ord, Show)

-- | The current CPU mode.
data CPUMode = ModeHalt | ModeStop | ModeNormal deriving (Eq, Ord, Show, Bounded, Enum)

-- | The internal CPU state.
data CPUState = CPUState {
    registers :: ForeignPtr RegisterFile
  , cpuMode :: CPUMode
} deriving (Eq, Ord, Show)

-- | The CPU monad.
type CPU a = ReaderT Memory (StateT CPUState IO) a

-- | Read data from the register file.
readRegister :: Storable a => Int -> CPU a
readRegister offset = do
  regs <- gets registers
  liftIO $ withForeignPtr regs $ flip peekByteOff offset

-- | Write data to the register file.
writeRegister :: Storable a => Int -> a -> CPU ()
writeRegister offset value = do
  regs <- gets registers
  liftIO $ withForeignPtr regs $ \ptr -> pokeByteOff ptr offset value

-- | Read a single register.
readR8 :: Register8 -> CPU Word8
readR8 = readRegister . offsetR8

-- | Write a single register.
writeR8 :: Register8 -> Word8 -> CPU ()
writeR8 register = writeRegister $ offsetR8 register

-- | Get the offset in the register file of a single register.
offsetR8 :: Register8 -> Int
offsetR8 RegA = offsetA
offsetR8 RegB = offsetB
offsetR8 RegC = offsetC
offsetR8 RegD = offsetD
offsetR8 RegE = offsetE
offsetR8 RegF = offsetF
offsetR8 RegH = offsetH
offsetR8 RegL = offsetL

-- | Read a 16-bit register.
readR16 :: Register16 -> CPU Word16
readR16 = readRegister . offsetR16

-- | Write a 16-bit register.
writeR16 :: Register16 -> Word16 -> CPU ()
writeR16 register = writeRegister $ offsetR16 register

-- | Get the offset in the register file of a register pair.
offsetR16 :: Register16 -> Int
offsetR16 RegBC = offsetC
offsetR16 RegDE = offsetE
offsetR16 RegHL = offsetL
offsetR16 RegSP = offsetSP

-- | Read the PC register.
readPC :: CPU Word16
readPC = readRegister offsetPC

-- | Write the PC register.
writePC :: Word16 -> CPU ()
writePC = writeRegister offsetPC

-- | Read the AF register.
readAF :: CPU Word16
readAF = readRegister offsetF

-- | Write the AF register.
writeAF :: Word16 -> CPU ()
writeAF = writeRegister offsetF

-- | Read from a 'Operand8'. Returns a list of addresses that were read from.
readOperand8 :: Operand8 -> CPU (Word8, [Word16])
readOperand8 (R8 register) = (, []) <$> readR8 register
readOperand8 (I8 value   ) = pure (value, [])
readOperand8 HLI           = do
  hl    <- readR16 RegHL
  mem   <- ask
  value <- liftIO $ readByte mem hl
  pure (value, [hl])

-- | Read from a 'SmallOperand8'.  Returns a list of addresses that were read from.
readSmallOperand8 :: SmallOperand8 -> CPU (Word8, [Word16])
readSmallOperand8 (SmallR8 register) = (, []) <$> readR8 register
readSmallOperand8 SmallHLI           = readOperand8 HLI

-- | Write to a 'SmallOperand8'.  Returns a list of addresses that were written to.
writeSmallOperand8 :: SmallOperand8 -> Word8 -> CPU [Word16]
writeSmallOperand8 (SmallR8 register) value = [] <$ writeR8 register value
writeSmallOperand8 SmallHLI           value = do
  hl  <- readR16 RegHL
  mem <- ask
  liftIO $ writeMem mem hl value
  pure [hl]

type Flag = Word8
flagZ, flagN, flagH, flagCY :: Flag
flagZ = 0x80
flagN = 0x40
flagH = 0x20
flagCY = 0x10

-- | Check if a flag is set.
testFlag :: Flag -> CPU Bool
testFlag flag = do
  f <- readR8 RegF
  pure $ f .&. flag /= 0

-- | Check if a condition code is true.
testCondition :: Maybe ConditionCode -> CPU Bool
testCondition Nothing       = pure True
testCondition (Just CondNZ) = not <$> testFlag flagZ
testCondition (Just CondZ ) = testFlag flagZ
testCondition (Just CondNC) = not <$> testFlag flagCY
testCondition (Just CondC ) = testFlag flagCY

-- | Set all the flags.
setFlags :: Word8 -> CPU ()
setFlags = setFlagsMask 0xF0

-- | Set some flags.
setFlagsMask
  :: Word8 -- ^ bitmask containing flags to set.
  -> Word8 -- ^ new flags values.
  -> CPU ()
setFlagsMask mask flags = do
  oldFlags <- readR8 RegF
  writeR8 RegF $ (oldFlags .&. complement mask) .|. (flags .&. mask)

-- | An arithmetic operation.
data ArithmeticOp = OpAdd | OpSub deriving (Eq, Ord, Show, Bounded, Enum)

-- | Perform an arithmetic operation and adjust the flags.
adder8 :: ArithmeticOp -> Word8 -> Word8 -> Bool -> CPU Word8
adder8 op a1 a2 carry = do
  let (wa1, wa2) = (fromIntegral a1 :: Word16, fromIntegral a2)
  let wr = case op of
        OpAdd -> wa1 + wa2 + (if carry then 1 else 0)
        OpSub -> wa1 - wa2 - (if carry then 1 else 0)
  let r       = fromIntegral wr
  let carryH = (wa1 .&. 0x0010) `xor` (wa1 .&. 0x0010) /= (wr .&. 0x0010)
  let carryCY = (wa1 .&. 0x0100) `xor` (wa1 .&. 0x0100) /= (wr .&. 0x0100)
  setFlags
    $   (if r == 0 then flagZ else 0)
    .|. (if op == OpSub then flagN else 0)
    .|. (if carryH then flagH else 0)
    .|. (if carryCY then flagCY else 0)
  pure r

-- | Perform an increment operation and adjust the flags.
inc8 :: ArithmeticOp -> Word8 -> CPU Word8
inc8 op value = do
  let r = case op of
        OpAdd -> value + 1
        OpSub -> value - 1
  let carryH = (value .&. 0x10) /= (r .&. 0x010)
  setFlagsMask (flagZ .|. flagN .|. flagH)
    $   (if r == 0 then flagZ else 0)
    .|. (if op == OpSub then flagN else 0)
    .|. (if carryH then flagH else 0)
  pure r

-- | Embed a 'Decode' action inside the 'CPU' monad.
hoistDecode :: Decode a -> CPU a
hoistDecode action = do
  mem      <- ask
  pc       <- readPC
  (r, pc') <- liftIO $ runStateT (runReaderT action mem) pc
  writePC pc'
  pure r

-- | Fetch, decode, and execute a single instruction.
cpuStep :: CPU DebugInfo
cpuStep = do
  mem <- ask
  pc0 <- readPC
  hoistDecode decode >>= \case
    Nothing          -> error $ "Invalid instruction at " ++ show pc0
    Just instruction -> case instruction of
      -- LD r8 \<r8|im8|(HL)\>
      LD_R8 r8 o8 -> do
        (value, readAddress) <- readOperand8 o8
        writeR8 r8 value
        pure $ DebugInfo readAddress []
      -- LD (HL) r8
      LDHLI_R8 r8 -> do
        value <- readR8 r8
        hl    <- readR16 RegHL
        liftIO $ writeMem mem hl value
        pure $ DebugInfo [] [hl]
      -- LD (HL) im8
      LDHLI_I8 im8 -> do
        hl <- readR16 RegHL
        liftIO $ writeMem mem hl im8
        pure $ DebugInfo [] [hl]
      -- LD A (BC)
      LDA_BCI -> do
        bc    <- readR16 RegBC
        value <- liftIO $ readByte mem bc
        writeR8 RegA value
        pure $ DebugInfo [bc] []
      -- LD A (DE)
      LDA_DEI -> do
        de    <- readR16 RegDE
        value <- liftIO $ readByte mem de
        writeR8 RegA value
        pure $ DebugInfo [de] []
      -- LD A (C)
      LDA_CI -> do
        c <- readR8 RegC
        let addr = fromIntegral c + 0xFF00
        value <- liftIO $ readByte mem addr
        writeR8 RegA value
        pure $ DebugInfo [addr] []
      -- LD (C) A
      LDCI_A -> do
        c <- readR8 RegC
        let addr = fromIntegral c + 0xFF00
        value <- readR8 RegA
        liftIO $ writeMem mem addr value
        pure $ DebugInfo [] [addr]
      -- LD A (im8)
      LDA_I8I w8 -> do
        let addr = fromIntegral w8 + 0xFF00
        value <- liftIO $ readByte mem addr
        writeR8 RegA value
        pure $ DebugInfo [addr] []
      -- LD (im8) A
      LDI8I_A w8 -> do
        let addr = fromIntegral w8 + 0xFF00
        value <- readR8 RegA
        liftIO $ writeMem mem addr value
        pure $ DebugInfo [] [addr]
      -- LD A (im16)
      LDA_I16I w16 -> do
        value <- liftIO $ readByte mem w16
        writeR8 RegA value
        pure $ DebugInfo [w16] []
      -- LD (im16) A
      LDI16I_A w16 -> do
        value <- readR8 RegA
        liftIO $ writeMem mem w16 value
        pure $ DebugInfo [] [w16]
      -- LD A (HL++)
      LDA_INC -> do
        hl    <- readR16 RegHL
        value <- liftIO $ readByte mem hl
        writeR8 RegA value
        writeR16 RegHL (hl + 1)
        pure $ DebugInfo [hl] []
      -- LD A (HL--)
      LDA_DEC -> do
        hl    <- readR16 RegHL
        value <- liftIO $ readByte mem hl
        writeR8 RegA value
        writeR16 RegHL (hl - 1)
        pure $ DebugInfo [hl] []
      -- LD (BC) A
      LDBCI_A -> do
        value <- readR8 RegA
        addr  <- readR16 RegBC
        liftIO $ writeMem mem addr value
        pure $ DebugInfo [] [addr]
      -- LD (DE) A
      LDDEI_A -> do
        value <- readR8 RegA
        addr  <- readR16 RegDE
        liftIO $ writeMem mem addr value
        pure $ DebugInfo [] [addr]
      -- LD (HL++) A
      LDHLI_INC -> do
        value <- readR8 RegA
        hl    <- readR16 RegHL
        liftIO $ writeMem mem hl value
        writeR16 RegHL (hl + 1)
        pure $ DebugInfo [] [hl]
      -- LD (HL--) A
      LDHLI_DEC -> do
        value <- readR8 RegA
        hl    <- readR16 RegHL
        liftIO $ writeMem mem hl value
        writeR16 RegHL (hl - 1)
        pure $ DebugInfo [] [hl]
      -- LD r16 im16
      LD16_I16 r16 w16 -> do
        writeR16 r16 w16
        pure $ DebugInfo [] []
      -- LD HL SP
      LDSP -> do
        writeR16 RegHL =<< readR16 RegSP
        pure $ DebugInfo [] []
      -- PUSH r16
      PUSH r16 -> do
        sp <- readR16 RegSP
        let sp' = sp - 2
        value <- if r16 == RegSP then readAF else readR16 r16
        liftIO $ writeMem mem sp' value
        writeR16 RegSP sp'
        pure $ DebugInfo [] [sp']
      -- POP r16
      POP r16 -> do
        sp     <- readR16 RegSP
        valueL <- liftIO $ fromIntegral <$> readByte mem sp
        valueH <- liftIO $ fromIntegral <$> readByte mem (sp + 1)
        let value = (valueH `unsafeShiftL` 8) .|. valueL
        if r16 == RegSP then writeAF value else writeR16 r16 value
        writeR16 RegSP (sp + 2)
        pure $ DebugInfo [sp] []
      -- LDHL SP im8
      LDHL i8 -> do
        sp <- fromIntegral <$> readR16 RegSP
        let wi8     = fromIntegral i8 :: Word32
        let wr      = sp + wi8
        let carryH = (sp .&. 0x00001000) `xor` (wi8 .&. 0x00001000) /= (wr .&. 0x00001000)
        let carryCY = (sp .&. 0x00010000) `xor` (wi8 .&. 0x00010000) /= (wr .&. 0x00010000)
        writeR16 RegHL $ fromIntegral wr
        setFlags $ (if carryCY then flagCY else 0) .|. (if carryH then flagH else 0)
        pure $ DebugInfo [] []
      -- LD (im16) SP
      LDI16I_SP w16 -> do
        sp <- readR16 RegSP
        liftIO $ writeMem mem w16 sp
        pure $ DebugInfo [] [w16, w16 + 1]
      -- ADD \<r8|im8|(HL)\>
      ADD o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        writeR8 RegA =<< adder8 OpAdd a arg False
        pure $ DebugInfo readAddr []
      -- ADC \<r8|im8|(HL)\>
      ADC o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        carry           <- testFlag flagCY
        writeR8 RegA =<< adder8 OpAdd a arg carry
        pure $ DebugInfo readAddr []
      -- SUB \<r8|im8|(HL)\>
      SUB o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        writeR8 RegA =<< adder8 OpSub a arg False
        pure $ DebugInfo readAddr []
      -- SBC \<r8|im8|(HL)\>
      SBC o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        carry           <- testFlag flagCY
        writeR8 RegA =<< adder8 OpSub a arg carry
        pure $ DebugInfo readAddr []
      -- AND \<r8|im8|(HL)\>
      AND o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        let r = a .&. arg
        writeR8 RegA r
        setFlags $ flagH .|. (if r == 0 then flagZ else 0)
        pure $ DebugInfo readAddr []
      -- OR \<r8|im8|(HL)\>
      OR o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        let r = a .|. arg
        writeR8 RegA r
        setFlags $ if r == 0 then flagZ else 0
        pure $ DebugInfo readAddr []
      -- XOR \<r8|im8|(HL)\>
      XOR o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        let r = a `xor` arg
        writeR8 RegA r
        setFlags $ if r == 0 then flagZ else 0
        pure $ DebugInfo readAddr []
      -- CP \<r8|im8|(HL)\>
      CP o8 -> do
        a               <- readR8 RegA
        (arg, readAddr) <- readOperand8 o8
        void $ adder8 OpSub a arg False
        pure $ DebugInfo readAddr []
      -- INC \<r8|(HL)\>
      INC so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        r                 <- inc8 OpAdd value
        wroteAddr         <- writeSmallOperand8 so8 r
        pure $ DebugInfo readAddr wroteAddr
      -- DEC \<r8|(HL)\>
      DEC so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        r                 <- inc8 OpSub value
        wroteAddr         <- writeSmallOperand8 so8 r
        pure $ DebugInfo readAddr wroteAddr
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
        pure $ DebugInfo [] []
      -- ADD SP im8
      ADDSP i8 -> do
        sp <- fromIntegral <$> readR16 RegSP
        let wi8     = fromIntegral i8
        let wr      = wi8 + sp :: Word32
        let carryH = (sp .&. 0x00001000) `xor` (wi8 .&. 0x00001000) /= (wr .&. 0x00001000)
        let carryCY = (sp .&. 0x00010000) `xor` (wi8 .&. 0x00010000) /= (wr .&. 0x00010000)
        writeR16 RegSP $ fromIntegral wr
        setFlags $ (if carryH then flagH else 0) .|. (if carryCY then flagCY else 0)
        pure $ DebugInfo [] []
      -- INC16 r16
      INC16 r16 -> do
        writeR16 r16 =<< (+ 1) <$> readR16 r16
        pure $ DebugInfo [] []
      -- DEC16 r16
      DEC16 r16 -> do
        writeR16 r16 =<< (subtract 1) <$> readR16 r16
        pure $ DebugInfo [] []
      -- RLCA
      RLCA -> do
        a <- readR8 RegA
        setFlags $ if a .&. 0x80 /= 0 then flagCY else 0
        writeR8 RegA $ rotateL a 1
        pure $ DebugInfo [] []
      -- RLA
      RLA -> do
        a <- readR8 RegA
        let ir = rotateL a 1
        hasCY <- testFlag flagCY
        setFlags $ if a .&. 0x80 /= 0 then flagCY else 0
        writeR8 RegA $ if hasCY then ir .|. 0x01 else ir .&. 0xFE
        pure $ DebugInfo [] []
      -- RRCA
      RRCA -> do
        a <- readR8 RegA
        setFlags $ if a .&. 0x01 /= 0 then flagCY else 0
        writeR8 RegA $ rotateR a 1
        pure $ DebugInfo [] []
      -- RRA
      RRA -> do
        a <- readR8 RegA
        let ir = rotateR a 1
        hasCY <- testFlag flagCY
        setFlags $ if a .&. 0x01 /= 0 then flagCY else 0
        writeR8 RegA $ if hasCY then ir .|. 0x80 else ir .&. 0x7F
        pure $ DebugInfo [] []
      -- RLC \<r8|(HL)\>
      RLC so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        setFlags $ (if value .&. 0x80 /= 0 then flagCY else 0) .|. (if value == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 $ rotateL value 1
        pure $ DebugInfo readAddr writeAddr
      -- RL \<r8|(HL)\>
      RL so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        let ir = rotateL value 1
        hasCY <- testFlag flagCY
        let r = if hasCY then ir .|. 0x01 else ir .&. 0xFE
        setFlags $ (if value .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 r
        pure $ DebugInfo readAddr writeAddr
      -- RRC \<r8|(HL)\>
      RRC so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if value == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 $ rotateR value 1
        pure $ DebugInfo readAddr writeAddr
      -- RR \<r8|(HL)\>
      RR so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        let ir = rotateR value 1
        hasCY <- testFlag flagCY
        let r = if hasCY then ir .|. 0x80 else ir .&. 0x7F
        setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 r
        pure $ DebugInfo readAddr writeAddr
      -- SLA \<r8|(HL)\>
      SLA so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        let r = value `unsafeShiftL` 1
        setFlags $ (if value .&. 0x80 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 r
        pure $ DebugInfo readAddr writeAddr
      -- SRA \<r8|(HL)\>
      SRA so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        let r = (fromIntegral value `unsafeShiftR` 1) :: Int8
        setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 $ fromIntegral r
        pure $ DebugInfo readAddr writeAddr
      -- SRL \<r8|(HL)\>
      SRL so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        let r = value `unsafeShiftR` 1
        setFlags $ (if value .&. 0x01 /= 0 then flagCY else 0) .|. (if r == 0 then flagZ else 0)
        writeAddr <- writeSmallOperand8 so8 r
        pure $ DebugInfo readAddr writeAddr
      -- SWAP \<r8|(HL)\>
      SWAP so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        setFlags $ if value == 0 then flagZ else 0
        writeAddr <- writeSmallOperand8 so8 $ (value `unsafeShiftR` 4) .|. (value `unsafeShiftL` 4)
        pure $ DebugInfo readAddr writeAddr
      -- BIT b \<r8|(HL)\>
      BIT w8 so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        setFlagsMask (flagH .|. flagN .|. flagZ)
          $   flagH
          .|. (if value `testBit` (fromIntegral w8) then 0 else 1)
        pure $ DebugInfo readAddr []
      -- SET b \<r8|(HL)\>
      SET w8 so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        writeAddr         <- writeSmallOperand8 so8 (value `setBit` (fromIntegral w8))
        pure $ DebugInfo readAddr writeAddr
      -- RES b \<r8|(HL)\>
      RES w8 so8 -> do
        (value, readAddr) <- readSmallOperand8 so8
        writeAddr         <- writeSmallOperand8 so8 (value `clearBit` (fromIntegral w8))
        pure $ DebugInfo readAddr writeAddr
      -- JP cc im16
      JP cc w16 -> do
        shouldJump <- testCondition cc
        when shouldJump $ writePC w16
        pure $ DebugInfo [] []
      -- JR cc im8
      JR cc i8 -> do
        shouldJump <- testCondition cc
        when shouldJump $ do
          pc <- readPC
          writePC $ pc + fromIntegral i8 + 2
        pure $ DebugInfo [] []
      -- JP (HL)
      JPI -> do
        writePC =<< readR16 RegHL
        pure $ DebugInfo [] []
      -- CALL cc im16
      CALL cc w16 -> do
        shouldCall <- testCondition cc
        if not shouldCall
          then pure $ DebugInfo [] []
          else do
            sp <- readR16 RegSP
            let sp' = sp - 2
            pc <- readPC
            liftIO $ writeMem mem sp' pc
            writePC w16
            writeR16 RegSP sp'
            pure $ DebugInfo [] [sp', sp' + 1]
      -- RETI
      RETI -> do
        -- TODO: reset master interrupt enable flag
        sp  <- readR16 RegSP
        pcL <- liftIO $ fromIntegral <$> readByte mem sp
        pcH <- liftIO $ fromIntegral <$> readByte mem (sp + 1)
        writePC $ (pcH `unsafeShiftL` 8) .|. pcL
        writeR16 RegSP $ sp + 2
        pure $ DebugInfo [sp, sp + 1] []
      -- RET cc
      RET cc -> do
        shouldCall <- testCondition cc
        if not shouldCall
          then pure $ DebugInfo [] []
          else do
            sp  <- readR16 RegSP
            pcL <- liftIO $ fromIntegral <$> readByte mem sp
            pcH <- liftIO $ fromIntegral <$> readByte mem (sp + 1)
            writePC $ (pcH `unsafeShiftL` 8) .|. pcL
            writeR16 RegSP $ sp + 2
            pure $ DebugInfo [sp, sp + 1] []
      -- RST t
      RST w8 -> do
        sp <- readR16 RegSP
        pc <- readPC
        let sp' = sp - 2
        liftIO $ writeMem mem sp' pc
        writeR16 RegSP sp'
        writePC $ 8 * fromIntegral w8
        pure $ DebugInfo [] [sp', sp' + 1]
      -- DAA
      DAA -> error "DAA not implemented"
      -- CPL
      CPL -> do
        writeR8 RegA =<< complement <$> readR8 RegA
        pure $ DebugInfo [] []
      -- NOP
      NOP  -> pure $ DebugInfo [] []
      -- HALT
      HALT -> do
        modify $ \cpu -> cpu { cpuMode = ModeHalt }
        pure $ DebugInfo [] []
      -- STOP
      STOP -> do
        modify $ \cpu -> cpu { cpuMode = ModeStop }
        pure $ DebugInfo [] []
