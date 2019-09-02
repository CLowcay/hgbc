module GBC.CPUSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Int
import           Data.Word
import           GBC.CPU
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

withNoChangeToRegisters :: CPU () -> CPU ()
withNoChangeToRegisters computation = forM_ [0 .. 3] $ \flags -> do
  writeR8 RegF $ (flags `unsafeShiftL` 4) .|. 0x0A
  registerFile0 <- getRegisterFile
  computation
  registerFile1 <- getRegisterFile
  liftIO $ registerFile1 `shouldBe` registerFile0

withFlagsUpdate :: Word8 -> Word8 -> CPU () -> CPU ()
withFlagsUpdate mask expected computation = forM_ [0 .. 0xF] $ \flags -> do
  let flags0 = (flags `unsafeShiftL` 4) .|. 0x0A
  writeR8 RegF flags0
  registerFile0 <- getRegisterFile
  computation
  flags1 <- readR8 RegF
  writeR8 RegF flags0
  registerFile1 <- getRegisterFile
  liftIO $ do
    registerFile1 `shouldBe` registerFile0
    (flags1 .&. mask) `shouldBe` expected

preservingR8 :: Register8 -> CPU () -> CPU ()
preservingR8 register computation = do
  v <- readR8 register
  computation
  writeR8 register v

preserving16 :: Register16 -> CPU () -> CPU ()
preserving16 register computation = do
  v <- readR16 register
  computation
  writeR16 register v

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
        withNoChangeToRegisters $ preservingR8 dest $ do
          flags0 <- readR8 RegF
          ev     <- executeInstruction $ LD_R8 dest (R8 source)
          r      <- readR8 dest
          when (dest == RegF) $ writeR8 RegF flags0
          liftIO $ verifyLoad r (if source == RegF then flags0 else 0x42) ev noReadWrite
  forM_ [minBound .. maxBound] $ \dest -> do
    it ("works for LD " ++ show dest ++ ", 0x42")
      $ withNewCPU
      $ withNoChangeToRegisters
      $ preservingR8 dest
      $ do
          flags0 <- readR8 RegF
          ev     <- executeInstruction $ LD_R8 dest (I8 0x42)
          r      <- readR8 dest
          when (dest == RegF) $ writeR8 RegF flags0
          liftIO $ verifyLoad r 0x42 ev noReadWrite
    it ("works for LD " ++ show dest ++ ", (HL)") $ withNewCPU $ do
      writeR16 RegHL 0xC000
      withNoChangeToRegisters $ preservingR8 dest $ do
        flags0 <- readR8 RegF
        writeMem 0xC000 (0x42 :: Word8)
        ev <- executeInstruction $ LD_R8 dest HLI
        r  <- readR8 dest
        when (dest == RegF) $ writeR8 RegF flags0
        liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])

ldhli_r8 :: Spec
ldhli_r8 = forM_ [minBound .. maxBound] $ \source ->
  it ("works for LD (HL), " ++ show source) $ withNewCPU $ do
    writeR8 source 0x42
    writeR16 RegHL 0xC000
    withNoChangeToRegisters $ do
      ev       <- executeInstruction $ LDHLI_R8 source
      r        <- readByte 0xC000
      expected <- case source of
        RegH -> pure 0xC0
        RegL -> pure 0x00
        RegF -> readR8 RegF
        _    -> pure 0x42
      liftIO $ verifyLoad r expected ev (didWrite [0xC000])

ldhli_i8 :: Spec
ldhli_i8 = it "works for LD (HL) 0x42" $ withNewCPU $ do
  writeR16 RegHL 0xC000
  withNoChangeToRegisters $ do
    ev <- executeInstruction $ LDHLI_I8 0x42
    r  <- readByte 0xC000
    liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])

simpleLoads :: Spec
simpleLoads = do
  describe "LDA_BCI" $ it "works for LD A, (BC)" $ withNewCPU $ do
    writeR16 RegBC 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_BCI
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])
  describe "LDA_DEI" $ it "works for LD A, (DE)" $ withNewCPU $ do
    writeR16 RegDE 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_DEI
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])
  describe "LDA_CI" $ it "works for LD A, (C)" $ withNewCPU $ do
    writeR8 RegC 0x44
    writeMem 0xFF44 (0x42 :: Word8)
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction LDA_CI
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xFF44])
  describe "LDCI_A" $ it "works for LD (C), A" $ withNewCPU $ do
    writeR8 RegC 0x44
    writeR8 RegA 0x42
    withNoChangeToRegisters $ do
      ev <- executeInstruction LDCI_A
      r  <- readByte 0xFF44
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xFF44])
  describe "LDA_I8I" $ it "works for LD A, (i8)" $ withNewCPU $ do
    writeMem 0xFF44 (0x42 :: Word8)
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDA_I8I 0x44
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xFF44])
  describe "LDI8I_A" $ it "works for LD (i8), A" $ withNewCPU $ do
    writeR8 RegA 0x42
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDI8I_A 0x44
      r  <- readByte 0xFF44
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xFF44])
  describe "LDA_I16I" $ it "works for LD A, (i16)" $ withNewCPU $ do
    writeMem 0xC000 (0x42 :: Word8)
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDA_I16I 0xC000
      r  <- readR8 RegA
      liftIO $ verifyLoad r 0x42 ev (didRead [0xC000])
  describe "LDI16I_A" $ it "works for LD (i16), A" $ withNewCPU $ do
    writeR8 RegA 0x42
    withNoChangeToRegisters $ preservingR8 RegA $ do
      ev <- executeInstruction $ LDI16I_A 0xC000
      r  <- readByte 0xC000
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])
  describe "LDA_INC" $ it "works for LD A, (HL++)" $ withNewCPU $ do
    writeR16 RegHL 0xC000
    writeMem 0xC000 (0x42 :: Word8)
    withNoChangeToRegisters $ preservingR8 RegA $ do
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
    withNoChangeToRegisters $ preservingR8 RegA $ do
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
    withNoChangeToRegisters $ do
      ev <- executeInstruction LDBCI_A
      r  <- readByte 0xC000
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])
  describe "LDDEI_A" $ it "works for LD (DE), A" $ withNewCPU $ do
    writeR16 RegDE 0xC000
    writeR8 RegA 0x42
    withNoChangeToRegisters $ do
      ev <- executeInstruction LDDEI_A
      r  <- readByte 0xC000
      liftIO $ verifyLoad r 0x42 ev (didWrite [0xC000])
  describe "LDHLI_INC" $ it "works for LD (HL++), A" $ withNewCPU $ do
    writeR16 RegHL 0xC000
    writeR8 RegA 0x42
    withNoChangeToRegisters $ do
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
    withNoChangeToRegisters $ do
      ev <- executeInstruction LDHLI_DEC
      r  <- readByte 0xC000
      hl <- readR16 RegHL
      writeR16 RegHL 0xC000
      liftIO $ do
        verifyLoad r 0x42 ev (didWrite [0xC000])
        hl `shouldBe` 0xBFFF
  describe "LDSP" $ it "works for LD SP, HL" $ withNewCPU $ do
    writeR16 RegHL 0x4242
    withNoChangeToRegisters $ preserving16 RegSP $ do
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
    withNoChangeToRegisters $ preserving16 RegSP $ do
      flags0 <- readR8 RegF
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
  withNoChangeToRegisters $ preserving16 RegSP $ specialPreserving16 dest $ do
    sp  <- readR16 RegSP
    ev  <- executeInstruction $ POP dest
    sp' <- readR16 RegSP
    r   <- case dest of
      RegSP -> do
        l <- readR8 RegF
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
    f <- readR8 RegF
    void computation
    writeR8 RegF f
    writeR8 RegA a
  specialPreserving16 r computation = do
    v <- readR16 r
    void computation
    writeR16 r v

ldi16i_sp :: Spec
ldi16i_sp = it "works for LD (0xC000), SP" $ withNewCPU $ do
  writeR16 RegSP 0xDEAD
  withNoChangeToRegisters $ do
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
  withFlagsUpdate (flagCY .|. flagH .|. flagZ .|. flagN)
                  ((if expectCY then flagCY else 0) .|. (if expectH then flagH else 0))
    $ preserving16 RegHL
    $ do
        ev <- executeInstruction $ LDHL e
        hl <- readR16 RegHL
        liftIO $ do
          ev `shouldBe` noReadWrite
          hl `shouldBe` sp + fromIntegral e
