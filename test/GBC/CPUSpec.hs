module GBC.CPUSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Int
import           Data.Word
import           GBC.CPU
import           Common
import           GBC.ISA
import           GBC.Memory
import           GBC.ROM
import           Test.Hspec
import qualified Data.ByteString               as B

blankROM :: ROM
blankROM = ROM $ B.replicate (32 * 1024 * 1024) 0

withNewCPU :: CPU () -> IO ()
withNewCPU computation = do
  mem <- initMemory blankROM
  cpu <- initCPU
  void $ runCPU mem cpu computationWithVerification
 where
  computationWithVerification = do
    computation
    registersConsistent

  registersConsistent = do
    bc <- readR16 RegBC
    de <- readR16 RegDE
    hl <- readR16 RegHL
    b  <- readR8 RegB
    c  <- readR8 RegC
    d  <- readR8 RegD
    e  <- readR8 RegE
    h  <- readR8 RegH
    l  <- readR8 RegL
    liftIO $ do
      fromIntegral (bc `unsafeShiftR` 8) `shouldBe` b
      fromIntegral (de `unsafeShiftR` 8) `shouldBe` d
      fromIntegral (hl `unsafeShiftR` 8) `shouldBe` h
      fromIntegral (bc .&. 0x00FF) `shouldBe` c
      fromIntegral (de .&. 0x00FF) `shouldBe` e
      fromIntegral (hl .&. 0x00FF) `shouldBe` l

withAllFlagCombos :: CPU () -> CPU ()
withAllFlagCombos computation = forM_ [0 .. 0xF] $ \flags -> do
  writeF $ (flags `unsafeShiftL` 4) .|. 0x0A
  computation

withNoChangeToRegisters :: CPU () -> CPU ()
withNoChangeToRegisters computation = do
  registerFile0 <- getRegisterFile
  computation
  registerFile1 <- getRegisterFile
  liftIO $ registerFile1 `shouldBe` registerFile0

withFlagsUpdate :: Word8 -> Word8 -> CPU () -> CPU ()
withFlagsUpdate mask expected computation = do
  registerFile0 <- getRegisterFile
  flags0        <- readF
  computation
  flags1 <- readF
  writeF flags0
  registerFile1 <- getRegisterFile
  liftIO $ do
    registerFile1 `shouldBe` registerFile0
    (flags1 .&. mask) `shouldBe` expected

withFlagsUpdateZ :: Word8 -> (Word8, Word8) -> CPU Word8 -> CPU ()
withFlagsUpdateZ mask (expected, expectedCarry) computation = do
  registerFile0 <- getRegisterFile
  flags0        <- readF
  hasCarry      <- testFlag flagCY
  a1            <- computation
  flags1        <- readF
  writeF flags0
  registerFile1 <- getRegisterFile
  liftIO $ do
    registerFile1 `shouldBe` registerFile0
    (flags1 .&. mask)
      `shouldBe` ((if hasCarry then expectedCarry else expected) .|. if a1 == 0 then flagZ else 0)

preservingR8 :: Register8 -> CPU a -> CPU a
preservingR8 register computation = do
  v <- readR8 register
  r <- computation
  writeR8 register v
  pure r

preservingR16 :: Register16 -> CPU a -> CPU a
preservingR16 register computation = do
  v <- readR16 register
  r <- computation
  writeR16 register v
  pure r

verifyLoad :: (Num n, Show n, Eq n) => n -> n -> BusEvent -> BusEvent -> IO ()
verifyLoad r expected ev evExpected = do
  r `shouldBe` expected
  ev `shouldBe` evExpected

spec :: Spec
spec = do
  describe "LD_R8"    ld_r8
  describe "LDHLI_R8" ldhli_r8
  describe "LDHLI_I8" ldhli_i8
  simpleLoads
  describe "LDHL" $ do
    ldhl 0xC000 0     False False
    ldhl 0xC000 1     False False
    ldhl 0xC000 42    False False
    ldhl 0xC000 (-1)  True  False
    ldhl 0xC000 (-42) True  False
    ldhl 0xC0FF 1     False False
    ldhl 0x8FFF 1     False True
    ldhl 0xFFFF 127   True  True
  describe "LDI16I_SP" ldi16i_sp
  describe "PUSH"      push
  describe "POP"       pop
  describe "ADD" $ do
    arithmeticOp ADD (0   , 1)    (1   , 1)    0
    arithmeticOp ADD (0   , 0)    (0   , 0)    0
    arithmeticOp ADD (0x08, 0x08) (0x10, 0x10) flagH
    arithmeticOp ADD (0x80, 0x80) (0   , 0)    flagCY
    arithmeticOp ADD (0x88, 0x88) (0x10, 0x10) (flagCY .|. flagH)
    arithmeticOp ADD (0xFF, 0x01) (0   , 0)    (flagCY .|. flagH)
    arithmeticOpA (ADD $ R8 RegA) 0x88 (0x10, 0x10) (flagCY .|. flagH, flagCY .|. flagH)
    arithmeticOpA (ADD $ R8 RegA) 0x80 (0   , 0)    (flagCY          , flagCY)
  describe "ADC" $ do
    arithmeticOp ADC (0   , 1)    (1   , 2)    0
    arithmeticOp ADC (0   , 0)    (0   , 1)    0
    arithmeticOp ADC (0x08, 0x08) (0x10, 0x11) flagH
    arithmeticOp ADC (0x80, 0x80) (0   , 1)    flagCY
    arithmeticOp ADC (0x88, 0x88) (0x10, 0x11) (flagCY .|. flagH)
    arithmeticOp ADC (0xFF, 0x01) (0   , 1)    (flagCY .|. flagH)
    arithmeticOpA (ADC $ R8 RegA) 0x88 (0x10, 0x11) (flagCY .|. flagH, flagCY .|. flagH)
    arithmeticOpA (ADC $ R8 RegA) 0x80 (0   , 1)    (flagCY          , flagCY)
  describe "SUB" $ do
    arithmeticOp SUB (0x3E, 0x3E) (0   , 0)    flagN
    arithmeticOp SUB (3   , 1)    (2   , 2)    flagN
    arithmeticOp SUB (0x3E, 0x0F) (0x2F, 0x2F) (flagN .|. flagH)
    arithmeticOp SUB (0x3E, 0x40) (0xFE, 0xFE) (flagN .|. flagCY)
    arithmeticOp SUB (0   , 1)    (0xFF, 0xFF) (flagN .|. flagCY .|. flagH)
    arithmeticOpA (SUB $ R8 RegA) 0x88 (0, 0) (flagN, flagN)
  describe "SBC" $ do
    arithmeticOp SBC (3, 2) (1   , 0)    flagN
    arithmeticOp SBC (0, 1) (0xFF, 0xFE) (flagN .|. flagCY .|. flagH)
    arithmeticOpA (SBC $ R8 RegA) 0x88 (0, 0xFF) (flagN, flagN .|. flagCY .|. flagH)
  describe "AND" $ do
    arithmeticOp AND (0x5A, 0x3F) (0x1A, 0x1A) flagH
    arithmeticOp AND (0x01, 0x02) (0   , 0)    flagH
    arithmeticOpA (AND $ R8 RegA) 0x5B (0x5B, 0x5B) (flagH, flagH)
  describe "OR" $ do
    arithmeticOp OR (0x5A, 0x0F) (0x5F, 0x5F) 0
    arithmeticOp OR (0   , 0)    (0   , 0)    0
    arithmeticOpA (OR $ R8 RegA) 0x5B (0x5B, 0x5B) (0, 0)
  describe "XOR" $ do
    arithmeticOp XOR (0xFF, 0x0F) (0xF0, 0xF0) 0
    arithmeticOp XOR (0x3D, 0x3D) (0   , 0)    0
    arithmeticOpA (XOR $ R8 RegA) 0x5B (0, 0) (0, 0)
  describe "CP" $ do
    cp (0x3E, 0x3E) (flagZ .|. flagN)
    cp (3   , 1)    flagN
    cp (0x3E, 0x0F) (flagN .|. flagH)
    cp (0x3E, 0x40) (flagN .|. flagCY)
    cp (0   , 1)    (flagN .|. flagCY .|. flagH)
    cpa
  describe "INC" $ do
    incdec INC 0    1    0
    incdec INC 0x0F 0x10 flagH
    incdec INC 0xFF 0    (flagZ .|. flagH)
  describe "DEC" $ do
    incdec DEC 1    0    (flagN .|. flagZ)
    incdec DEC 0x10 0x0F (flagN .|. flagH)
    incdec DEC 0    0xFF (flagN .|. flagH)
  describe "ADDHL" $ do
    addHL 0      0      0      0
    addHL 0      1      1      0
    addHL 0x00FF 1      0x0100 0
    addHL 0x0800 0x0800 0x1000 flagH
    addHL 0x8000 0x8000 0x0000 flagCY
    addHL 0x8800 0x8800 0x1000 (flagCY .|. flagH)
    addHLHL 0      0      0
    addHLHL 0x0800 0x1000 flagH
    addHLHL 0x8000 0x0000 flagCY
    addHLHL 0x8800 0x1000 (flagCY .|. flagH)
  describe "ADDSP" $ do
    addSP 300    1    301    0
    addSP 300    (-1) 299    (flagCY .|. flagH)
    addSP 0x00FF 1    0x0100 0
    addSP 0x0FFF 1    0x1000 flagH
    addSP 0xFFFF 1    0x0000 (flagH .|. flagCY)
  describe "INC16" $ do
    incdec16 INC16 0      1
    incdec16 INC16 0xFFFF 0
  describe "DEC16" $ do
    incdec16 DEC16 3 2
    incdec16 DEC16 1 0
    incdec16 DEC16 0 0xFFFF
  describe "RLCA" $ do
    rotA RLCA (False, 1)    (False, 2)
    rotA RLCA (True , 1)    (False, 2)
    rotA RLCA (False, 0x81) (True , 0x03)
  describe "RLA" $ do
    rotA RLA (False, 1)    (False, 2)
    rotA RLA (True , 1)    (False, 3)
    rotA RLA (False, 0x81) (True , 0x02)
    rotA RLA (True , 0x81) (True , 0x03)
  describe "RRCA" $ do
    rotA RRCA (False, 1) (True , 0x80)
    rotA RRCA (True , 1) (True , 0x80)
    rotA RRCA (False, 2) (False, 1)
    rotA RRCA (True , 2) (False, 1)
  describe "RRA" $ do
    rotA RRA (False, 1) (True , 0)
    rotA RRA (True , 1) (True , 0x80)
    rotA RRA (False, 2) (False, 1)
    rotA RRA (True , 2) (False, 0x81)
  describe "RLC" $ do
    shiftRotate RLC (False, 1)    (False, 2)
    shiftRotate RLC (True , 1)    (False, 2)
    shiftRotate RLC (False, 0x81) (True , 0x03)
  describe "RL" $ do
    shiftRotate RL (False, 1)    (False, 2)
    shiftRotate RL (True , 1)    (False, 3)
    shiftRotate RL (False, 0x81) (True , 0x02)
    shiftRotate RL (True , 0x81) (True , 0x03)
  describe "RRC" $ do
    shiftRotate RRC (False, 1) (True , 0x80)
    shiftRotate RRC (True , 1) (True , 0x80)
    shiftRotate RRC (False, 2) (False, 1)
    shiftRotate RRC (True , 2) (False, 1)
  describe "RR" $ do
    shiftRotate RR (False, 1) (True , 0)
    shiftRotate RR (True , 1) (True , 0x80)
    shiftRotate RR (False, 2) (False, 1)
    shiftRotate RR (True , 2) (False, 0x81)
  describe "SLA" $ do
    shiftRotate SLA (False, 1)    (False, 2)
    shiftRotate SLA (False, 0x80) (True , 0)
  describe "SRA" $ do
    shiftRotate SRA (False, 1)    (True , 0)
    shiftRotate SRA (False, 0x80) (False, 0xC0)
  describe "SRL" $ do
    shiftRotate SRL (False, 1)    (True , 0)
    shiftRotate SRL (False, 0x80) (False, 0x40)
  describe "SWAP" $ do
    shiftRotate SWAP (False, 0xDE) (False, 0xED)
    shiftRotate SWAP (True , 0xDE) (False, 0xED)
  bitTest
  describe "SET" $ setReset SET True
  describe "RES" $ setReset RES False

noReadWrite :: BusEvent
noReadWrite = BusEvent [] []

didRead :: [Word16] -> BusEvent
didRead r = BusEvent r []

didWrite :: [Word16] -> BusEvent
didWrite = BusEvent []

ld_r8 :: Spec
ld_r8 = do
  forM_ [ (sr, dr) | sr <- [minBound .. maxBound], dr <- [minBound .. maxBound] ]
    $ \(source, dest) -> it ("works for LD " ++ show dest ++ ", " ++ show source) $ withNewCPU $ do
        writeR8 source 0x42
        withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 dest $ do
          ev <- executeInstruction $ LD_R8 dest (R8 source)
          r  <- readR8 dest
          liftIO $ verifyLoad r 0x42 ev noReadWrite
  forM_ [minBound .. maxBound] $ \dest -> do
    it ("works for LD " ++ show dest ++ ", 0x42")
      $ withNewCPU
      $ withAllFlagCombos
      $ withNoChangeToRegisters
      $ preservingR8 dest
      $ do
          ev <- executeInstruction $ LD_R8 dest (I8 0x42)
          r  <- readR8 dest
          liftIO $ verifyLoad r 0x42 ev noReadWrite
    it ("works for LD " ++ show dest ++ ", (HL)") $ withNewCPU $ do
      writeR16 RegHL 0xC000
      withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 dest $ do
        writeMem 0xC000 (0x42 :: Word8)
        ev <- executeInstruction $ LD_R8 dest HLI
        r  <- readR8 dest
        liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])

ldhli_r8 :: Spec
ldhli_r8 = forM_ [minBound .. maxBound] $ \source ->
  it ("works for LD (HL), " ++ show source) $ withNewCPU $ do
    writeR8 source 0x42
    writeR16 RegHL 0xC000
    withAllFlagCombos $ withNoChangeToRegisters $ do
      ev       <- executeInstruction $ LDHLI_R8 source
      r        <- readByte 0xC000
      expected <- case source of
        RegH -> pure 0xC0
        RegL -> pure 0x00
        _    -> pure 0x42
      liftIO $ verifyLoad r expected ev (didWrite [0xC000])

ldhli_i8 :: Spec
ldhli_i8 = it "works for LD (HL) 0x42" $ withNewCPU $ do
  writeR16 RegHL 0xC000
  withAllFlagCombos $ withNoChangeToRegisters $ do
    ev <- executeInstruction $ LDHLI_I8 0x42
    r  <- readByte 0xC000
    liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])

simpleLoads :: Spec
simpleLoads = do
  describe "LDA_BCI" $ it "works for LD A, (BC)" $ withNewCPU $ do
    writeR16 RegBC 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_BCI
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])
  describe "LDA_DEI" $ it "works for LD A, (DE)" $ withNewCPU $ do
    writeR16 RegDE 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_DEI
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])
  describe "LDA_CI" $ it "works for LD A, (C)" $ withNewCPU $ do
    writeR8 RegC 0x44
    writeMem 0xFF44 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_CI
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xFF44])
  describe "LDCI_A" $ it "works for LD (C), A" $ withNewCPU $ do
    writeR8 RegC 0x44
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ do
      ev <- executeInstruction LDCI_A
      r  <- readByte 0xFF44
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xFF44])
  describe "LDA_I8I" $ it "works for LD A, (i8)" $ withNewCPU $ do
    writeMem 0xFF44 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDA_I8I 0x44
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xFF44])
  describe "LDI8I_A" $ it "works for LD (i8), A" $ withNewCPU $ do
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDI8I_A 0x44
      r  <- readByte 0xFF44
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xFF44])
  describe "LDA_I16I" $ it "works for LD A, (i16)" $ withNewCPU $ do
    writeMem 0xC000 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDA_I16I 0xC000
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])
  describe "LDI16I_A" $ it "works for LD (i16), A" $ withNewCPU $ do
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDI16I_A 0xC000
      r  <- readByte 0xC000
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])
  describe "LDA_INC" $ it "works for LD A, (HL++)" $ withNewCPU $ do
    writeR16 RegHL 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_INC
      r  <- readR8 RegA
      hl <- readR16 RegHL
      writeR16 RegHL 0xC000
      liftIO $ do
        verifyLoad r 0x42 ev (didRead [0xC000])
        hl `shouldBe` 0xC001
  describe "LDA_DEC" $ it "works for LD A, (HL--)" $ withNewCPU $ do
    writeR16 RegHL 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_DEC
      r  <- readR8 RegA
      hl <- readR16 RegHL
      writeR16 RegHL 0xC000
      liftIO $ do
        verifyLoad r 0x42 ev (didRead [0xC000])
        hl `shouldBe` 0xBFFF
  describe "LDBCI_A" $ it "works for LD (BC), A" $ withNewCPU $ do
    writeR16 RegBC 0xC000
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ do
      ev <- executeInstruction LDBCI_A
      r  <- readByte 0xC000
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])
  describe "LDDEI_A" $ it "works for LD (DE), A" $ withNewCPU $ do
    writeR16 RegDE 0xC000
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ do
      ev <- executeInstruction LDDEI_A
      r  <- readByte 0xC000
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])
  describe "LDHLI_INC" $ it "works for LD (HL++), A" $ withNewCPU $ do
    writeR16 RegHL 0xC000
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ do
      ev <- executeInstruction LDHLI_INC
      r  <- readByte 0xC000
      hl <- readR16 RegHL
      writeR16 RegHL 0xC000
      liftIO $ do
        verifyLoad r 0x42 ev (didWrite [0xC000])
        hl `shouldBe` 0xC001
  describe "LDHLI_DEC" $ it "works for LD (HL--), A" $ withNewCPU $ do
    writeR16 RegHL 0xC000
    writeR8 RegA 0x42
    withAllFlagCombos $ withNoChangeToRegisters $ do
      ev <- executeInstruction LDHLI_DEC
      r  <- readByte 0xC000
      hl <- readR16 RegHL
      writeR16 RegHL 0xC000
      liftIO $ do
        verifyLoad r 0x42 ev (didWrite [0xC000])
        hl `shouldBe` 0xBFFF
  describe "LDSP" $ it "works for LD SP, HL" $ withNewCPU $ do
    writeR16 RegHL 0x4242
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR16 RegSP $ do
      ev <- executeInstruction LDSP
      r  <- readR16 RegSP
      liftIO $ verifyLoad r 0x4242 ev noReadWrite

push :: Spec
push = forM_ [minBound .. maxBound] $ \source ->
  it ("works for PUSH " ++ show source) $ withNewCPU $ do
    writeR16 RegSP 0xFFF0
    case source of
      RegSP -> writeR8 RegA 0x42
      _     -> writeR16 source 0x0102
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR16 RegSP $ do
      flags0 <- readF
      ev     <- executeInstruction $ PUSH source
      sp'    <- readR16 RegSP
      l      <- readByte sp'
      h      <- readByte (sp' + 1)
      liftIO $ do
        sp' `shouldBe` 0xFFEE
        ev `shouldBe` didWrite [sp', sp' + 1]
        l `shouldBe` (if source == RegSP then flags0 else 0x02)
        h `shouldBe` (if source == RegSP then 0x42 else 0x01)

pop :: Spec
pop = forM_ [minBound .. maxBound] $ \dest -> it ("works for POP " ++ show dest) $ withNewCPU $ do
  writeR16 RegSP 0xFFF0
  writeMem 0xFFF0 (0x0102 :: Word16)
  withAllFlagCombos $ withNoChangeToRegisters $ preservingR16 RegSP $ specialPreserving16 dest $ do
    sp  <- readR16 RegSP
    ev  <- executeInstruction $ POP dest
    sp' <- readR16 RegSP
    r   <- case dest of
      RegSP -> do
        l <- readF
        h <- readR8 RegA
        pure $ (fromIntegral h `unsafeShiftL` 8) .|. fromIntegral l
      _ -> readR16 dest
    liftIO $ do
      sp' `shouldBe` 0xFFF2
      ev `shouldBe` didRead [sp, sp + 1]
      r `shouldBe` 0x0102
 where
  specialPreserving16 RegSP computation = do
    a <- readR8 RegA
    f <- readF
    void computation
    writeF f
    writeR8 RegA a
  specialPreserving16 r computation = do
    v <- readR16 r
    void computation
    writeR16 r v

ldi16i_sp :: Spec
ldi16i_sp = it "works for LD (0xC000), SP" $ withNewCPU $ do
  writeR16 RegSP 0xDEAD
  withAllFlagCombos $ withNoChangeToRegisters $ do
    ev <- executeInstruction $ LDI16I_SP 0xC000
    l  <- readByte 0xC000
    h  <- readByte 0xC001
    liftIO $ do
      l `shouldBe` 0xAD
      h `shouldBe` 0xDE
      ev `shouldBe` didWrite [0xC000, 0xC001]

ldhl :: Word16 -> Int8 -> Bool -> Bool -> Spec
ldhl sp e expectCY expectH = it ("works for LDHL SP, " ++ show e) $ withNewCPU $ do
  writeR16 RegSP sp
  withAllFlagCombos
    $ withFlagsUpdate allFlags ((if expectCY then flagCY else 0) .|. (if expectH then flagH else 0))
    $ preservingR16 RegHL
    $ do
        ev <- executeInstruction $ LDHL e
        hl <- readR16 RegHL
        liftIO $ do
          ev `shouldBe` noReadWrite
          hl `shouldBe` sp + fromIntegral e

allFlags :: Word8
allFlags = flagCY .|. flagH .|. flagZ .|. flagN

arithmeticOp :: (Operand8 -> Instruction) -> (Word8, Word8) -> (Word8, Word8) -> Word8 -> Spec
arithmeticOp instruction (a, r) (expected, expectedWithCarry) expectedFlags = do
  forM_ (filter (/= RegA) [minBound .. maxBound]) $ \register ->
    it ("works for " ++ format (instruction $ R8 register) ++ " " ++ show (a, r)) $ withNewCPU $ do
      writeR8 RegA     a
      writeR8 register r
      withAllFlagCombos
        $ withFlagsUpdateZ allFlags (expectedFlags, expectedFlags)
        $ preservingR8 register
        $ preservingR8 RegA
        $ do
            hasCarry <- testFlag flagCY
            ev       <- executeInstruction $ instruction $ R8 register
            a'       <- readR8 RegA
            liftIO $ do
              ev `shouldBe` noReadWrite
              a' `shouldBe` (if hasCarry then expectedWithCarry else expected)
            pure a'
  it ("works for " ++ format (instruction $ I8 r) ++ " " ++ show (a, r)) $ withNewCPU $ do
    writeR8 RegA a
    withAllFlagCombos
      $ withFlagsUpdateZ allFlags (expectedFlags, expectedFlags)
      $ preservingR8 RegA
      $ do
          hasCarry <- testFlag flagCY
          ev       <- executeInstruction $ instruction $ I8 r
          a'       <- readR8 RegA
          liftIO $ do
            ev `shouldBe` noReadWrite
            a' `shouldBe` (if hasCarry then expectedWithCarry else expected)
          pure a'
  it ("works for " ++ format (instruction HLI) ++ " " ++ show (a, r)) $ withNewCPU $ do
    writeR8 RegA a
    writeR16 RegHL 0xC000
    writeMem 0xC000 r
    withAllFlagCombos
      $ withFlagsUpdateZ allFlags (expectedFlags, expectedFlags)
      $ preservingR8 RegA
      $ do
          hasCarry <- testFlag flagCY
          ev       <- executeInstruction $ instruction HLI
          a'       <- readR8 RegA
          liftIO $ do
            ev `shouldBe` didRead [0xC000]
            a' `shouldBe` (if hasCarry then expectedWithCarry else expected)
          pure a'

cp :: (Word8, Word8) -> Word8 -> Spec
cp (a, r) expectedFlags = do
  forM_ (filter (/= RegA) [minBound .. maxBound]) $ \register ->
    it ("works for CP " ++ show register ++ " " ++ show (a, r)) $ withNewCPU $ do
      writeR8 RegA     a
      writeR8 register r
      withAllFlagCombos $ withFlagsUpdate allFlags expectedFlags $ do
        ev <- executeInstruction $ CP $ R8 register
        liftIO $ ev `shouldBe` noReadWrite
  it ("works for CP " ++ show r ++ " " ++ show (a, r)) $ withNewCPU $ do
    writeR8 RegA a
    withAllFlagCombos $ withFlagsUpdate allFlags expectedFlags $ do
      ev <- executeInstruction $ CP $ I8 r
      liftIO $ ev `shouldBe` noReadWrite
  it ("works for CP (HL) " ++ show (a, r)) $ withNewCPU $ do
    writeR8 RegA a
    writeR16 RegHL 0xC000
    withAllFlagCombos $ withFlagsUpdate allFlags expectedFlags $ do
      writeMem 0xC000 r
      ev <- executeInstruction $ CP HLI
      liftIO $ ev `shouldBe` didRead [0xC000]

cpa :: Spec
cpa = it "works for CP A" $ withNewCPU $ do
  writeR8 RegA 0x3C
  withAllFlagCombos $ withFlagsUpdate allFlags (flagN .|. flagZ) $ do
    ev <- executeInstruction $ CP $ R8 RegA
    liftIO $ ev `shouldBe` noReadWrite

arithmeticOpA :: Instruction -> Word8 -> (Word8, Word8) -> (Word8, Word8) -> Spec
arithmeticOpA instruction value (expected, expectedCarry) (expectedFlags, expectedCarryFlags) =
  it ("works for " ++ format instruction ++ " " ++ show value) $ withNewCPU $ do
    writeR8 RegA value
    withAllFlagCombos
      $ withFlagsUpdateZ allFlags (expectedFlags, expectedCarryFlags)
      $ preservingR8 RegA
      $ do
          hasCarry <- testFlag flagCY
          ev       <- executeInstruction instruction
          value'   <- readR8 RegA
          liftIO $ do
            ev `shouldBe` noReadWrite
            value' `shouldBe` if hasCarry then expectedCarry else expected
          pure value'

incdec :: (SmallOperand8 -> Instruction) -> Word8 -> Word8 -> Word8 -> Spec
incdec instruction value expected expectedFlags = do
  forM_ [minBound .. maxBound] $ \register ->
    it ("works for " ++ format (instruction $ SmallR8 register) ++ " (" ++ show value ++ ")")
      $ withNewCPU
      $ do
          writeR8 register value
          withAllFlagCombos
            . withFlagsUpdate (flagH .|. flagN .|. flagZ) expectedFlags
            $ preservingR8 register
            $ do
                ev     <- executeInstruction $ instruction $ SmallR8 register
                (r, _) <- readSmallOperand8 (SmallR8 register)
                liftIO $ do
                  ev `shouldBe` noReadWrite
                  r `shouldBe` expected
  it ("works for " ++ format (instruction SmallHLI) ++ " (" ++ show value ++ ")") $ withNewCPU $ do
    writeR16 RegHL 0xC000
    withAllFlagCombos $ withFlagsUpdate (flagH .|. flagN .|. flagZ) expectedFlags $ do
      writeMem 0xC000 value
      ev <- executeInstruction $ instruction SmallHLI
      r  <- readByte 0xC000
      liftIO $ do
        ev `shouldBe` BusEvent [0xC000] [0xC000]
        r `shouldBe` expected

addHL :: Word16 -> Word16 -> Word16 -> Word8 -> Spec
addHL value addend expected expectedFlags =
  forM_ (filter (/= RegHL) [minBound .. maxBound]) $ \register ->
    it ("works for ADD HL " ++ show register ++ " (" ++ show (value, addend) ++ ")")
      $ withNewCPU
      $ do
          writeR16 RegHL    value
          writeR16 register addend
          withAllFlagCombos
            $ withFlagsUpdate (flagCY .|. flagH .|. flagN) expectedFlags
            $ preservingR16 RegHL
            $ do
                ev <- executeInstruction $ ADDHL register
                r  <- readR16 RegHL
                liftIO $ do
                  ev `shouldBe` noReadWrite
                  r `shouldBe` expected

addHLHL :: Word16 -> Word16 -> Word8 -> Spec
addHLHL value expected expectedFlags =
  it ("works for ADD HL HL (" ++ show value ++ ")") $ withNewCPU $ do
    writeR16 RegHL value
    withAllFlagCombos
      $ withFlagsUpdate (flagCY .|. flagH .|. flagN) expectedFlags
      $ preservingR16 RegHL
      $ do
          ev <- executeInstruction $ ADDHL RegHL
          r  <- readR16 RegHL
          liftIO $ do
            ev `shouldBe` noReadWrite
            r `shouldBe` expected

addSP :: Word16 -> Int8 -> Word16 -> Word8 -> Spec
addSP value e expected expectedFlags =
  it ("works for ADD SP e (" ++ show e ++ ")") $ withNewCPU $ do
    writeR16 RegSP value
    withAllFlagCombos $ withFlagsUpdate allFlags expectedFlags $ preservingR16 RegSP $ do
      ev <- executeInstruction $ ADDSP e
      r  <- readR16 RegSP
      liftIO $ do
        ev `shouldBe` noReadWrite
        r `shouldBe` expected

incdec16 :: (Register16 -> Instruction) -> Word16 -> Word16 -> Spec
incdec16 instruction value expected = forM_ [minBound .. maxBound] $ \register ->
  it ("works for " ++ format (instruction register) ++ " (" ++ show value ++ ")") $ withNewCPU $ do
    writeR16 register value
    withAllFlagCombos $ withNoChangeToRegisters $ preservingR16 register $ do
      ev <- executeInstruction $ instruction register
      r  <- readR16 register
      liftIO $ do
        ev `shouldBe` noReadWrite
        r `shouldBe` expected

rotA :: Instruction -> (Bool, Word8) -> (Bool, Word8) -> Spec
rotA instruction (carry, value) (expectedCarry, expected) =
  it ("works for " ++ format instruction ++ " " ++ show (value, carry))
    $ withNewCPU
    $ withAllFlagCombos
    $ do
        writeR8 RegA value
        setFlagsMask flagCY (if carry then flagCY else 0)
        withFlagsUpdate allFlags (if expectedCarry then flagCY else 0) $ preservingR8 RegA $ do
          ev <- executeInstruction instruction
          r  <- readR8 RegA
          liftIO $ do
            ev `shouldBe` noReadWrite
            r `shouldBe` expected

dup :: a -> (a, a)
dup x = (x, x)

shiftRotate :: (SmallOperand8 -> Instruction) -> (Bool, Word8) -> (Bool, Word8) -> Spec
shiftRotate instruction (carry, value) (expectedCarry, expected) = do
  forM_ [minBound .. maxBound] $ \register ->
    it ("works for " ++ format (instruction $ SmallR8 register) ++ " " ++ show (carry, value))
      $ withNewCPU
      $ withAllFlagCombos
      $ do
          writeR8 register value
          setFlagsMask flagCY $ if carry then flagCY else 0
          withFlagsUpdateZ allFlags (dup $ if expectedCarry then flagCY else 0)
            $ preservingR8 register
            $ do
                ev <- executeInstruction $ instruction $ SmallR8 register
                r  <- readR8 register
                liftIO $ do
                  ev `shouldBe` noReadWrite
                  r `shouldBe` expected
                pure r
  it ("works for " ++ format (instruction SmallHLI) ++ show (carry, value))
    $ withNewCPU
    $ withAllFlagCombos
    $ do
        writeR16 RegHL 0xC000
        writeMem 0xC000 value
        setFlagsMask flagCY $ if carry then flagCY else 0
        withFlagsUpdateZ allFlags (dup $ if expectedCarry then flagCY else 0) $ do
          ev <- executeInstruction $ instruction SmallHLI
          r  <- readByte 0xC000
          liftIO $ do
            ev `shouldBe` BusEvent [0xC000] [0xC000]
            r `shouldBe` expected
          pure r

bitTest :: Spec
bitTest = do
  forM_ [ (r, i) | r <- [minBound .. maxBound], i <- [0 .. 7] ] $ \(register, bitIndex) ->
    it ("works for BIT " ++ show register ++ " " ++ show bitIndex)
      $ withNewCPU
      $ withAllFlagCombos
      $ do
          doTest (SmallR8 register) bitIndex (bit bitIndex)              0     noReadWrite
          doTest (SmallR8 register) bitIndex (complement $ bit bitIndex) flagZ noReadWrite
  forM_ [0 .. 7] $ \bitIndex ->
    it ("works for BIT (HL) " ++ show bitIndex) $ withNewCPU $ withAllFlagCombos $ do
      writeR16 RegHL 0xC000
      doTest SmallHLI bitIndex (bit bitIndex) 0 $ didRead [0xC000]
      doTest SmallHLI bitIndex (complement $ bit bitIndex) flagZ $ didRead [0xC000]

 where
  doTest op8 bitIndex value expectedFlags expectedBusEvent = do
    void $ writeSmallOperand8 op8 value
    withFlagsUpdate (flagH .|. flagN .|. flagZ) (flagH .|. expectedFlags) $ do
      ev <- executeInstruction $ BIT (fromIntegral bitIndex) op8
      liftIO $ ev `shouldBe` expectedBusEvent

setReset :: (Word8 -> SmallOperand8 -> Instruction) -> Bool -> Spec
setReset instruction doSet = do
  forM_ [ (r, i) | r <- [minBound .. maxBound], i <- [0 .. 7] ] $ \(register, bitIndex) ->
    it ("works for " ++ format (instruction (fromIntegral bitIndex) (SmallR8 register)))
      $ withNewCPU
      $ withAllFlagCombos
      $ do
          doTest (SmallR8 register) bitIndex 0 (if doSet then bit bitIndex else 0) noReadWrite
          doTest (SmallR8 register)
                 bitIndex
                 0xFF
                 (if doSet then 0xFF else complement (bit bitIndex))
                 noReadWrite
  forM_ [0 .. 7] $ \bitIndex ->
    it ("works for " ++ format (instruction (fromIntegral bitIndex) SmallHLI))
      $ withNewCPU
      $ withAllFlagCombos
      $ do
          writeR16 RegHL 0xC000
          doTest SmallHLI bitIndex 0 (if doSet then bit bitIndex else 0)
            $ BusEvent [0xC000] [0xC000]
          doTest SmallHLI bitIndex 0xFF (if doSet then 0xFF else complement (bit bitIndex))
            $ BusEvent [0xC000] [0xC000]

 where
  doTest op8 bitIndex value result expectedBusEvent = do
    void $ writeSmallOperand8 op8 value
    withNoChangeToRegisters $ preserving op8 $ do
      ev     <- executeInstruction $ instruction (fromIntegral bitIndex) op8
      (r, _) <- readSmallOperand8 op8
      liftIO $ do
        ev `shouldBe` expectedBusEvent
        r `shouldBe` result
  preserving SmallHLI    = id
  preserving (SmallR8 r) = preservingR8 r
