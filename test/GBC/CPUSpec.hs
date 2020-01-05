{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module GBC.CPUSpec where

import           Common
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Traversable
import           Data.Word
import           GBC.CPU
import           GBC.CPU.Decode
import           GBC.CPU.ISA
import           GBC.Graphics.VRAM
import           GBC.Memory
import           GBC.Mode
import           GBC.Primitive
import           GBC.ROM
import           GBC.Registers
import           Test.Hspec
import qualified Data.ByteString               as B

spec :: Spec
spec = do
  describe "Loads" loads
  describe "Arithmetic8" arithmetic8
  describe "Arithmetic16" arithmetic16
  describe "rotateAndShift" rotateAndShift
  describe "bitOperations" bitOperations
  describe "jumps" jumps
  describe "callAndReturn" callAndReturn
  describe "Miscellaneous" miscellaneous
  describe "BCD" bcd
  describe "Interrupts" interrupts

blankROM :: ROM
blankROM = let size = 32 * 1024 * 1024 in ROM "testRom" (blankHeader size) (B.replicate size 0)

blankHeader :: Int -> Header
blankHeader romSize = Header { startAddress          = 0
                             , nintendoCharacterData = ""
                             , gameTitle             = ""
                             , gameCode              = ""
                             , cgbSupport            = CGBCompatible
                             , makerCode             = ""
                             , sgbSupport            = GBOnly
                             , cartridgeType         = CartridgeType Nothing False False
                             , romSize               = romSize
                             , externalRAM           = 0
                             , destination           = Overseas
                             , oldLicenseCode        = 0
                             , maskROMVersion        = 0
                             }

data CPUTestState = CPUTestState {
    testCPU :: !CPUState
  , testMemory :: !Memory
}

instance HasCPU CPUTestState where
  forCPUState = testCPU

instance HasMemory CPUTestState where
  forMemory = testMemory

withNewCPU :: CPUM CPUTestState () -> IO ()
withNewCPU computation = mdo
  vram   <- initVRAM DMG
  portIF <- newPort 0x00 0x1F alwaysUpdate
  portIE <- newPort 0x00 0xFF alwaysUpdate
  mem    <- initMemory blankROM vram ((IF, portIF) : cpuPorts cpu) portIE DMG
  cpu    <- initCPU portIF portIE DMG
  void $ runReaderT checkingFlags $ CPUTestState cpu mem
 where
  checkingFlags = for_ [0 .. 15] $ \flags -> do
    reset
    let flags0 = flags .<<. 4
    writeF flags0
    computationWithVerification
    flags1 <- readF
    liftIO $ flags1 `shouldBe` flags0

  computationWithVerification = do
    registers0 <- getRegisterFile
    mode0      <- getMode
    clocks0    <- getCPUCycleClocks

    runCPUM computation

    registers1 <- getRegisterFile
    mode1      <- getMode
    clocks1    <- getCPUCycleClocks

    registersConsistent
    liftIO $ do
      registers1 `shouldBe` registers0
      mode0 `shouldBe` mode1
      clocks0 `shouldBe` clocks1

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
    f  <- readF
    liftIO $ do
      f .&. 0x0F `shouldBe` 0
      fromIntegral (bc .>>. 8) `shouldBe` b
      fromIntegral (de .>>. 8) `shouldBe` d
      fromIntegral (hl .>>. 8) `shouldBe` h
      fromIntegral (bc .&. 0x00FF) `shouldBe` c
      fromIntegral (de .&. 0x00FF) `shouldBe` e
      fromIntegral (hl .&. 0x00FF) `shouldBe` l

alteringRegisters :: [RegisterR] -> CPUM CPUTestState () -> CPUM CPUTestState ()
alteringRegisters rs computation = do
  registers <- CPUM getRegisterFile
  computation
  CPUM $ for_ rs $ \case
    RegA -> writeR8 RegA (regA registers)
    RegB -> writeR8 RegB (regB registers)
    RegC -> writeR8 RegC (regC registers)
    RegD -> writeR8 RegD (regD registers)
    RegE -> writeR8 RegE (regE registers)
    RegH -> writeR8 RegH (regH registers)
    RegL -> writeR8 RegL (regL registers)

alteringSSRegisters :: [RegisterSS] -> CPUM CPUTestState () -> CPUM CPUTestState ()
alteringSSRegisters rs computation = do
  registers <- CPUM getRegisterFile
  computation
  CPUM $ for_ rs $ \case
    RegBC -> do
      writeR8 RegB (regB registers)
      writeR8 RegC (regC registers)
    RegDE -> do
      writeR8 RegD (regD registers)
      writeR8 RegE (regE registers)
    RegHL -> do
      writeR8 RegH (regH registers)
      writeR8 RegL (regL registers)
    RegSP -> writeR16 RegSP (regSP registers)

alteringQQRegisters :: [RegisterQQ] -> CPUM CPUTestState () -> CPUM CPUTestState ()
alteringQQRegisters rs computation = do
  registers <- CPUM getRegisterFile
  computation
  CPUM $ for_ rs $ \case
    PushPopBC -> do
      writeR8 RegB (regB registers)
      writeR8 RegC (regC registers)
    PushPopDE -> do
      writeR8 RegD (regD registers)
      writeR8 RegE (regE registers)
    PushPopHL -> do
      writeR8 RegH (regH registers)
      writeR8 RegL (regL registers)
    PushPopAF -> do
      writeR8 RegA (regA registers)
      writeF (regF registers)

alteringSP :: CPUM CPUTestState () -> CPUM CPUTestState ()
alteringSP computation = do
  sp <- CPUM $ readR16 RegSP
  computation
  CPUM $ writeR16 RegSP sp

alteringPC :: CPUM CPUTestState () -> CPUM CPUTestState ()
alteringPC computation = do
  pc <- CPUM readPC
  computation
  CPUM $ writePC pc

alteringMode :: CPUM CPUTestState () -> CPUM CPUTestState ()
alteringMode computation = do
  mode0 <- CPUM getMode
  computation
  CPUM $ setMode mode0

alteringCPUCycleClocks :: CPUM CPUTestState () -> CPUM CPUTestState ()
alteringCPUCycleClocks computation = do
  clocks <- CPUM getCPUCycleClocks
  computation
  CPUM $ setCPUCycleClocks clocks

alteringFlags :: Word8 -> CPUM CPUTestState () -> CPUM CPUTestState ()
alteringFlags mask computation = do
  flags0 <- CPUM readF
  computation
  flags1 <- CPUM readF
  CPUM $ writeF ((flags0 .&. mask) .|. (flags1 .&. complement mask))

notAlteringFlags :: Word8 -> CPUM CPUTestState () -> CPUM CPUTestState ()
notAlteringFlags mask computation = do
  flags0 <- CPUM readF
  computation
  flags1 <- CPUM readF
  liftIO ((flags1 .&. mask) `shouldBe` (flags0 .&. mask))

withPC :: Word16 -> CPUM CPUTestState () -> CPUM CPUTestState ()
withPC address computation = do
  pc0 <- CPUM readPC
  CPUM $ writePC address
  computation
  CPUM $ writePC pc0

withIME :: Bool -> CPUM CPUTestState () -> CPUM CPUTestState ()
withIME ime computation = do
  ime0 <- CPUM testIME
  CPUM $ if ime then setIME else clearIME
  computation
  CPUM $ if ime0 then setIME else clearIME

withMode :: CPUMode -> CPUM CPUTestState () -> CPUM CPUTestState ()
withMode mode computation = do
  mode0 <- CPUM getMode
  CPUM $ setMode mode
  computation
  CPUM $ setMode mode0

withValueAt :: RegisterSS -> Word16 -> Word8 -> CPUM CPUTestState () -> CPUM CPUTestState ()
withValueAt ss address value computation = alteringSSRegisters [ss] $ do
  CPUM $ do
    writeR16 ss address
    writeByte address value
  computation

withValue16At :: RegisterSS -> Word16 -> Word16 -> CPUM CPUTestState () -> CPUM CPUTestState ()
withValue16At ss address value computation = alteringSSRegisters [ss] $ do
  CPUM $ do
    writeR16 ss address
    writeWord address value
  computation

withValueAtC :: Word8 -> Word8 -> CPUM CPUTestState () -> CPUM CPUTestState ()
withValueAtC address value computation = alteringRegisters [RegC] $ do
  CPUM $ do
    writeR8 RegC address
    writeByte (0xFF00 + fromIntegral address) value
  computation

withValuesInRegisters :: [(RegisterR, Word8)] -> CPUM CPUTestState () -> CPUM CPUTestState ()
withValuesInRegisters rvs computation = alteringRegisters (fst <$> rvs) $ do
  for_ rvs $ \(r, v) -> CPUM $ writeR8 r v
  computation

withValuesInSSRegisters :: [(RegisterSS, Word16)] -> CPUM CPUTestState () -> CPUM CPUTestState ()
withValuesInSSRegisters rvs computation = alteringSSRegisters (fst <$> rvs) $ do
  for_ rvs $ \(r, v) -> CPUM $ writeR16 r v
  computation

withValuesInQQRegisters :: [(RegisterQQ, Word16)] -> CPUM CPUTestState () -> CPUM CPUTestState ()
withValuesInQQRegisters rvs computation = alteringQQRegisters (fst <$> rvs) $ do
  for_ rvs $ \(r, v) -> CPUM $ writeR16qq r v
  computation

setCondition :: ConditionCode -> CPUM CPUTestState ()
setCondition CondC  = CPUM $ setFlagsMask flagCY flagCY
setCondition CondNC = CPUM $ setFlagsMask flagCY 0
setCondition CondZ  = CPUM $ setFlagsMask flagZ flagZ
setCondition CondNZ = CPUM $ setFlagsMask flagZ 0

clearCondition :: ConditionCode -> CPUM CPUTestState ()
clearCondition CondC  = CPUM $ setFlagsMask flagCY 0
clearCondition CondNC = CPUM $ setFlagsMask flagCY flagCY
clearCondition CondZ  = CPUM $ setFlagsMask flagZ 0
clearCondition CondNZ = CPUM $ setFlagsMask flagZ flagZ

registerShouldBe :: RegisterR -> Word8 -> CPUM CPUTestState ()
registerShouldBe r expected = do
  v <- CPUM $ readR8 r
  liftIO (v `shouldBe` expected)

registerSSShouldBe :: RegisterSS -> Word16 -> CPUM CPUTestState ()
registerSSShouldBe r expected = do
  v <- CPUM $ readR16 r
  liftIO (v `shouldBe` expected)

registerQQShouldBe :: RegisterQQ -> Word16 -> CPUM CPUTestState ()
registerQQShouldBe r expected = do
  v <- CPUM $ readR16qq r
  liftIO (v `shouldBe` expected)

shouldHaveCycles :: CPUM CPUTestState Int -> Int -> CPUM CPUTestState ()
shouldHaveCycles instruction expectedCycles = do
  cycles <- instruction
  liftIO (cycles `shouldBe` expectedCycles)

atAddressShouldBe :: Word16 -> Word8 -> CPUM CPUTestState ()
atAddressShouldBe address value = do
  v <- CPUM $ readByte address
  liftIO (v `shouldBe` value)

expectFlags :: Word8 -> Word8 -> CPUM CPUTestState ()
expectFlags mask expected = do
  flags <- CPUM readF
  liftIO ((flags .&. mask) `shouldBe` (expected .&. mask))

expectPC :: Word16 -> CPUM CPUTestState ()
expectPC expected = do
  pc <- CPUM readPC
  liftIO (pc `shouldBe` expected)

expectMode :: CPUMode -> CPUM CPUTestState ()
expectMode expected = do
  mode <- CPUM getMode
  liftIO (mode `shouldBe` expected)

expectCPUCycleClocks :: Int -> CPUM CPUTestState ()
expectCPUCycleClocks expected = do
  clocks <- CPUM getCPUCycleClocks
  liftIO (clocks `shouldBe` expected)

expectIME :: Bool -> CPUM CPUTestState ()
expectIME expected = do
  ime <- CPUM testIME
  liftIO (ime `shouldBe` expected)

buildFlags :: Bool -> Bool -> Bool -> Bool -> Word8
buildFlags cy h n z =
  (if cy then flagCY else 0)
    .|. (if h then flagH else 0)
    .|. (if n then flagN else 0)
    .|. (if z then flagZ else 0)

allFlags :: Word8
allFlags = 0xF0

allConditions :: [ConditionCode]
allConditions = [minBound .. maxBound]

allRegisters :: [RegisterR]
allRegisters = [minBound .. maxBound]

allSSRegisters :: [RegisterSS]
allSSRegisters = [minBound .. maxBound]

allQQRegisters :: [RegisterQQ]
allQQRegisters = [minBound .. maxBound]

-- | Check if there is a carry into the specified bit when performing a binary
-- operation on two values.
carryIntoBit
  :: (Integral a, Integral b, Bits a, Bits b)
  => Int
  -> a
  -> b
  -> (Word32 -> Word32 -> Word32)
  -> Bool
carryIntoBit i a b op =
  ((fromIntegral a `clearBit` i) `op` (fromIntegral b `clearBit` i)) `testBit` i

loads :: Spec
loads = do
  describe "LD r, r" $ for_ [ (r, r') | r <- allRegisters, r' <- allRegisters ] $ \(r, r') ->
    it ("Works for LD " <> format r <> ", " <> format r')
      $ withNewCPU
      $ alteringRegisters [r]
      $ withValuesInRegisters [(r', 42)]
      $ do
          ldrr r r' `shouldHaveCycles` 1
          r `registerShouldBe` 42

  describe "LD r, n" $ for_ allRegisters $ \r ->
    it ("Works for LD " <> format r <> ", 42") $ withNewCPU $ alteringRegisters [r] $ do
      ldrn r 42 `shouldHaveCycles` 2
      r `registerShouldBe` 42

  describe "LD r, (HL)" $ for_ allRegisters $ \r ->
    it ("Works for LD " <> format r <> ", (HL)")
      $ withNewCPU
      $ alteringRegisters [r]
      $ withValueAt RegHL 0xC000 42
      $ do
          ldrHL r `shouldHaveCycles` 2
          r `registerShouldBe` 42
          RegH `registerShouldBe` (if r == RegH then 42 else 0xC0)
          RegL `registerShouldBe` (if r == RegL then 42 else 0)

  describe "LD (HL), r" $ for_ allRegisters $ \r ->
    it ("Works for LD (HL), " <> format r)
      $ withNewCPU
      $ withValuesInRegisters [(r, 42)]
      $ withValueAt RegHL 0xC0D0 32
      $ do
          ldHLr r `shouldHaveCycles` 2
          let expected = case r of
                RegH -> 0xC0
                RegL -> 0xD0
                _    -> 42
          r `registerShouldBe` expected
          RegH `registerShouldBe` 0xC0
          RegL `registerShouldBe` 0xD0
          0xC0D0 `atAddressShouldBe` expected

  describe "LD (HL), n" $ it "Works for LD (HL), 42" $ withNewCPU $ withValueAt RegHL 0xC000 32 $ do
    ldHLn 42 `shouldHaveCycles` 3
    0xC000 `atAddressShouldBe` 42
    RegH `registerShouldBe` 0xC0
    RegL `registerShouldBe` 0

  describe "LD A, (BC)"
    $ it "Works for LD A, (BC)"
    $ withNewCPU
    $ alteringRegisters [RegA]
    $ withValueAt RegBC 0xC000 42
    $ do
        ldaBC `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        0xC000 `atAddressShouldBe` 42
        RegB `registerShouldBe` 0xC0
        RegC `registerShouldBe` 0

  describe "LD A, (DE)"
    $ it "Works for LD A, (DE)"
    $ withNewCPU
    $ alteringRegisters [RegA]
    $ withValueAt RegDE 0xC000 42
    $ do
        ldaDE `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        0xC000 `atAddressShouldBe` 42
        RegD `registerShouldBe` 0xC0
        RegE `registerShouldBe` 0

  describe "LD A, (C)"
    $ it "Works for LD A, (C)"
    $ withNewCPU
    $ alteringRegisters [RegA]
    $ withValueAtC 0x80 42
    $ do
        ldaC `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        0xFF80 `atAddressShouldBe` 42
        RegC `registerShouldBe` 0x80

  describe "LD (C), A"
    $ it "Works for LD (C), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ withValueAtC 0x80 32
    $ do
        ldCa `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegC `registerShouldBe` 0x80
        0xFF80 `atAddressShouldBe` 42

  describe "LD A, (n)" $ it "Works for LD A, (80)" $ withNewCPU $ alteringRegisters [RegA] $ do
    CPUM $ writeByte 0xFF80 42
    ldan 0x80 `shouldHaveCycles` 3
    RegA `registerShouldBe` 42
    0xFF80 `atAddressShouldBe` 42

  describe "LD (n), A"
    $ it "Works for LD (80), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ do
        CPUM $ writeByte 0xFF80 32
        ldna 0x80 `shouldHaveCycles` 3
        RegA `registerShouldBe` 42
        0xFF80 `atAddressShouldBe` 42

  describe "LD A, (nn)" $ it "Works for LD A, (C000)" $ withNewCPU $ alteringRegisters [RegA] $ do
    CPUM $ writeByte 0xC000 42
    ldann 0xC000 `shouldHaveCycles` 4
    RegA `registerShouldBe` 42
    0xC000 `atAddressShouldBe` 42

  describe "LD (nn), A"
    $ it "Works for LD (C000), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ do
        CPUM $ writeByte 0xC000 32
        ldnna 0xC000 `shouldHaveCycles` 4
        RegA `registerShouldBe` 42
        0xC000 `atAddressShouldBe` 42

  describe "LD A, (HLI)"
    $ it "Works for LD A, (HLI)"
    $ withNewCPU
    $ alteringRegisters [RegA]
    $ withValueAt RegHL 0xC002 42
    $ do
        ldaHLI `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegH `registerShouldBe` 0xC0
        RegL `registerShouldBe` 0x03

  describe "LD A, (HLD)"
    $ it "Works for LD A, (HLD)"
    $ withNewCPU
    $ alteringRegisters [RegA]
    $ withValueAt RegHL 0xC002 42
    $ do
        ldaHLD `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegH `registerShouldBe` 0xC0
        RegL `registerShouldBe` 0x01

  describe "LD (BC), A"
    $ it "Works for LD (BC), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ withValueAt RegBC 0xC000 32
    $ do
        ldBCa `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegB `registerShouldBe` 0xC0
        RegC `registerShouldBe` 0x00
        0xC000 `atAddressShouldBe` 42

  describe "LD (DE), A"
    $ it "Works for LD (DE), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ withValueAt RegDE 0xC000 32
    $ do
        ldDEa `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegD `registerShouldBe` 0xC0
        RegE `registerShouldBe` 0x00
        0xC000 `atAddressShouldBe` 42

  describe "LD (HLI), A"
    $ it "Works for LD (HLI), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ withValueAt RegHL 0xC002 32
    $ do
        ldHLIa `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegH `registerShouldBe` 0xC0
        RegL `registerShouldBe` 0x03
        0xC002 `atAddressShouldBe` 42

  describe "LD (HLD), A"
    $ it "Works for LD (HLD), A"
    $ withNewCPU
    $ withValuesInRegisters [(RegA, 42)]
    $ withValueAt RegHL 0xC002 32
    $ do
        ldHLDa `shouldHaveCycles` 2
        RegA `registerShouldBe` 42
        RegH `registerShouldBe` 0xC0
        RegL `registerShouldBe` 0x01
        0xC002 `atAddressShouldBe` 42

  describe "LD dd, nn" $ for_ allSSRegisters $ \ss ->
    it ("Works for LD " <> format ss <> ", 4243") $ withNewCPU $ alteringSSRegisters [ss] $ do
      ldddnn ss 0x4232 `shouldHaveCycles` 3
      ss `registerSSShouldBe` 0x4232

  describe "LD SP, HL"
    $ it "Works for LD SP, HL"
    $ withNewCPU
    $ alteringSP
    $ withValuesInRegisters [(RegH, 0x42), (RegL, 0x32)]
    $ do
        ldSPHL `shouldHaveCycles` 2
        RegSP `registerSSShouldBe` 0x4232
        RegHL `registerSSShouldBe` 0x4232

  describe "PUSH qq" $ for_ allQQRegisters $ \qq ->
    it ("Works for PUSH " <> format qq)
      $ withNewCPU
      $ withValuesInQQRegisters [(qq, 0x4232)]
      $ withValue16At RegSP 0xFFF0 0x2221
      $ do
          push qq `shouldHaveCycles` 4
          RegSP `registerSSShouldBe` 0xFFEE
          0xFFEE `atAddressShouldBe` (if qq == PushPopAF then 0x30 else 0x32)
          0xFFEF `atAddressShouldBe` 0x42

  describe "POP qq" $ for_ allQQRegisters $ \qq ->
    it ("Works for POP " <> format qq)
      $ withNewCPU
      $ alteringQQRegisters [qq]
      $ withValue16At RegSP 0xFFEE 0x4232
      $ do
          pop qq `shouldHaveCycles` 3
          RegSP `registerSSShouldBe` 0xFFF0
          qq `registerQQShouldBe` (if qq == PushPopAF then 0x4230 else 0x4232)
          0xFFEE `atAddressShouldBe` 0x32
          0xFFEF `atAddressShouldBe` 0x42

  describe "LDHL SP, e" $ for_ [ (sp, e) | sp <- [0xF8FF, 0x0012], e <- [-32 .. 32] ] $ \(sp, e) ->
    it ("Works for LDHL SP, " <> formatHex e <> " ; (SP = " <> formatHex sp <> ")")
      $ withNewCPU
      $ withValuesInSSRegisters [(RegSP, sp)]
      $ alteringSSRegisters [RegHL]
      $ alteringFlags allFlags
      $ do
          ldhl e `shouldHaveCycles` 3
          RegHL `registerSSShouldBe` (sp + fromIntegral e)
          let carry  = carryIntoBit 8 sp e (+)
          let carryH = carryIntoBit 4 sp e (+)
          expectFlags allFlags ((if carry then flagCY else 0) .|. (if carryH then flagH else 0))

  describe "LD (nn), SP"
    $ it "Works for LD (C000), SP"
    $ withNewCPU
    $ withValuesInSSRegisters [(RegSP, 0xFFF0)]
    $ do
        CPUM $ writeWord 0xC000 0101
        ldnnSP 0xC000 `shouldHaveCycles` 5
        RegSP `registerSSShouldBe` 0xFFF0
        0xC000 `atAddressShouldBe` 0xF0
        0xC001 `atAddressShouldBe` 0xFF

arithmetic8 :: Spec
arithmetic8 = do
  describe "ADD A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for ADD A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              addr r `shouldHaveCycles` 1
              let a = if r == RegA then v else 0x11
              verifyArithmetic8 a v (+) False

  describe "ADD A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for ADD A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          addn v `shouldHaveCycles` 2
          verifyArithmetic8 0x11 v (+) False

  describe "ADD A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for ADD A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          addhl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          verifyArithmetic8 0x11 v (+) False

  describe "ADC A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for ADC A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              cy <- CPUM $ testFlag flagCY
              adcr r `shouldHaveCycles` 1
              let a = if r == RegA then v else 0x11
              verifyArithmetic8 a v (\x y -> x + y + (if cy then 1 else 0)) False

  describe "ADC A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for ADC A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          adcn v `shouldHaveCycles` 2
          verifyArithmetic8 0x11 v (\x y -> x + y + (if cy then 1 else 0)) False

  describe "ADC A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for ADC A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          adchl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          verifyArithmetic8 0x11 v (\x y -> x + y + (if cy then 1 else 0)) False

  describe "SUB A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for SUB A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              subr r `shouldHaveCycles` 1
              let a = if r == RegA then v else 0x11
              verifyArithmetic8 a v (-) True

  describe "SUB A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SUB A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          subn v `shouldHaveCycles` 2
          verifyArithmetic8 0x11 v (-) True

  describe "SUB A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SUB A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          subhl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          verifyArithmetic8 0x11 v (-) True

  describe "SBC A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for SBC A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              cy <- CPUM $ testFlag flagCY
              sbcr r `shouldHaveCycles` 1
              let a = if r == RegA then v else 0x11
              verifyArithmetic8 a v (\x y -> x - y - (if cy then 1 else 0)) True

  describe "SBC A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SBC A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          sbcn v `shouldHaveCycles` 2
          verifyArithmetic8 0x11 v (\x y -> x - y - (if cy then 1 else 0)) True

  describe "SBC A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SBC A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          sbchl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          verifyArithmetic8 0x11 v (\x y -> x - y - (if cy then 1 else 0)) True

  describe "AND A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for AND A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              andr r `shouldHaveCycles` 1
              let expected = v .&. if r == RegA then v else 0x11
              RegA `registerShouldBe` expected
              expectFlags allFlags (flagH .|. (if expected == 0 then flagZ else 0))

  describe "AND A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for AND A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          andn v `shouldHaveCycles` 2
          let expected = v .&. 0x11
          RegA `registerShouldBe` expected
          expectFlags allFlags (flagH .|. (if expected == 0 then flagZ else 0))

  describe "AND A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for AND A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          andhl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          let expected = v .&. 0x11
          expectFlags allFlags (flagH .|. (if expected == 0 then flagZ else 0))

  describe "OR A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for OR A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              orr r `shouldHaveCycles` 1
              let expected = v .|. if r == RegA then v else 0x11
              RegA `registerShouldBe` expected
              expectFlags allFlags (if expected == 0 then flagZ else 0)

  describe "OR A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for OR A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          orn v `shouldHaveCycles` 2
          let expected = v .|. 0x11
          RegA `registerShouldBe` expected
          expectFlags allFlags (if expected == 0 then flagZ else 0)

  describe "OR A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for OR A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          orhl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          let expected = v .|. 0x11
          expectFlags allFlags (if expected == 0 then flagZ else 0)

  describe "XOR A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for XOR A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              xorr r `shouldHaveCycles` 1
              let expected = v `xor` if r == RegA then v else 0x11
              RegA `registerShouldBe` expected
              expectFlags allFlags (if expected == 0 then flagZ else 0)

  describe "XOR A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for XOR A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          xorn v `shouldHaveCycles` 2
          let expected = v `xor` 0x11
          RegA `registerShouldBe` expected
          expectFlags allFlags (if expected == 0 then flagZ else 0)

  describe "XOR A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for XOR A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          xorhl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          let expected = v `xor` 0x11
          expectFlags allFlags (if expected == 0 then flagZ else 0)

  describe "CP A, r"
    $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ]
    $ \(r, v) ->
        it ("Works for CP A, " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInRegisters [(RegA, 0x11), (r, v)]
          $ alteringFlags allFlags
          $ do
              cpr r `shouldHaveCycles` 1
              let a = if r == RegA then v else 0x11
              verifyArithmetic8Flags a v (-) True

  describe "CP A, n" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for CP A, " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ alteringFlags allFlags
      $ do
          cpn v `shouldHaveCycles` 2
          verifyArithmetic8Flags 0x11 v (-) True

  describe "CP A, (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for CP A, (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, 0x11)]
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          cphl `shouldHaveCycles` 2
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` v
          verifyArithmetic8Flags 0x11 v (-) True

  describe "INC r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for INC " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags (flagH .|. flagN .|. flagZ)
      $ do
          incr r `shouldHaveCycles` 1
          let expected = v + 1
          r `registerShouldBe` expected
          expectFlags (flagH .|. flagN .|. flagZ)
                      (buildFlags False (carryIntoBit 4 v (1 :: Word8) (+)) False (expected == 0))

  describe "INC (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for INC (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags (flagH .|. flagN .|. flagZ)
      $ do
          inchl `shouldHaveCycles` 3
          let expected = v + 1
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` expected
          expectFlags (flagH .|. flagN .|. flagZ)
                      (buildFlags False (carryIntoBit 4 v (1 :: Word8) (+)) False (expected == 0))

  describe "DEC r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for DEC " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags (flagH .|. flagN .|. flagZ)
      $ do
          decr r `shouldHaveCycles` 1
          let expected = v - 1
          r `registerShouldBe` expected
          expectFlags (flagH .|. flagN .|. flagZ)
                      (buildFlags False (carryIntoBit 4 v (1 :: Word8) (-)) True (expected == 0))

  describe "DEC (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for DEC (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags (flagH .|. flagN .|. flagZ)
      $ do
          dechl `shouldHaveCycles` 3
          let expected = v - 1
          RegHL `registerSSShouldBe` 0xC000
          0xC000 `atAddressShouldBe` expected
          expectFlags (flagH .|. flagN .|. flagZ)
                      (buildFlags False (carryIntoBit 4 v (1 :: Word8) (-)) True (expected == 0))

verifyArithmetic8 :: Word8 -> Word8 -> (Word32 -> Word32 -> Word32) -> Bool -> CPUM CPUTestState ()
verifyArithmetic8 a v op n = do
  let expected = fromIntegral (fromIntegral a `op` fromIntegral v)
  RegA `registerShouldBe` expected
  verifyArithmetic8Flags a v op n

verifyArithmetic8Flags
  :: Word8 -> Word8 -> (Word32 -> Word32 -> Word32) -> Bool -> CPUM CPUTestState ()
verifyArithmetic8Flags a v op n = do
  let expected = fromIntegral (fromIntegral a `op` fromIntegral v) :: Word8
  expectFlags allFlags
              (buildFlags (carryIntoBit 8 a v op) (carryIntoBit 4 a v op) n (expected == 0))

arithmetic16 :: Spec
arithmetic16 = do
  describe "ADD HL, ss"
    $ for_ [ (ss, (v .<<. 8) + 1) | ss <- allSSRegisters, v <- [0 .. 255] ]
    $ \(ss, v) ->
        it ("Works for ADD HL, " <> format ss <> " ; (" <> format ss <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInSSRegisters [(RegHL, 0x11FF), (ss, v)]
          $ alteringFlags (flagCY .|. flagH .|. flagN)
          $ do
              addhlss ss `shouldHaveCycles` 2
              let a        = if ss == RegHL then v else 0x11FF
              let expected = v + a
              RegHL `registerSSShouldBe` expected
              expectFlags
                (flagCY .|. flagH .|. flagN)
                (buildFlags (carryIntoBit 16 a v (+)) (carryIntoBit 12 a v (+)) False False)

  describe "ADD SP, e" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for ADD SP, " <> formatHex v)
      $ withNewCPU
      $ withValuesInSSRegisters [(RegSP, 0x1111)]
      $ alteringFlags allFlags
      $ do
          addSP v `shouldHaveCycles` 4
          let a        = 0x1111
          let expected = a + fromIntegral v
          RegSP `registerSSShouldBe` expected
          expectFlags allFlags
                      (buildFlags (carryIntoBit 8 a v (+)) (carryIntoBit 4 a v (+)) False False)

  describe "INC ss"
    $ for_ [ (ss, (v .<<. 8) .|. 0xFF) | ss <- allSSRegisters, v <- [0 .. 255] ]
    $ \(ss, v) ->
        it ("Works for INC " <> format ss <> " ; (" <> format ss <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInSSRegisters [(ss, v)]
          $ do
              incss ss `shouldHaveCycles` 2
              ss `registerSSShouldBe` (v + 1)

  describe "DEC ss"
    $ for_ [ (ss, (v .<<. 8) .|. 0xFF) | ss <- allSSRegisters, v <- [0 .. 255] ]
    $ \(ss, v) ->
        it ("Works for DEC " <> format ss <> " ; (" <> format ss <> " = " <> formatHex v <> ")")
          $ withNewCPU
          $ withValuesInSSRegisters [(ss, v)]
          $ do
              decss ss `shouldHaveCycles` 2
              ss `registerSSShouldBe` (v - 1)

rotateAndShift :: Spec
rotateAndShift = do
  describe "RLCA" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RLCA ; (A = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, v)]
      $ alteringFlags allFlags
      $ do
          rlca `shouldHaveCycles` 1
          let expected = (v .<<. 1) .|. (v .>>. 7)
          RegA `registerShouldBe` expected
          expectFlags allFlags (if v `testBit` 7 then flagCY else 0)

  describe "RLA" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RLA ; (A = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, v)]
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          rla `shouldHaveCycles` 1
          let expected = (v .<<. 1) .|. (if cy then 1 else 0)
          RegA `registerShouldBe` expected
          expectFlags allFlags (if v `testBit` 7 then flagCY else 0)

  describe "RRCA" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RRCA ; (A = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, v)]
      $ alteringFlags allFlags
      $ do
          rrca `shouldHaveCycles` 1
          let expected = (v .>>. 1) .|. (v .<<. 7)
          RegA `registerShouldBe` expected
          expectFlags allFlags (if v `testBit` 0 then flagCY else 0)

  describe "RRA" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RRA ; (A = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(RegA, v)]
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          rra `shouldHaveCycles` 1
          let expected = (v .>>. 1) .|. (if cy then 0x80 else 0)
          RegA `registerShouldBe` expected
          expectFlags allFlags (if v `testBit` 0 then flagCY else 0)

  describe "RLC r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for RLC " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          rlcr r `shouldHaveCycles` 2
          let expected = (v .<<. 1) .|. (v .>>. 7)
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RL r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for RL " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          rlr r `shouldHaveCycles` 2
          let expected = (v .<<. 1) .|. (if cy then 1 else 0)
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RRC r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for RRC " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          rrcr r `shouldHaveCycles` 2
          let expected = (v .>>. 1) .|. (v .<<. 7)
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "RR r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for RR " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          rrr r `shouldHaveCycles` 2
          let expected = (v .>>. 1) .|. (if cy then 0x80 else 0)
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "RLC (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RLC (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          rlchl `shouldHaveCycles` 4
          let expected = (v .<<. 1) .|. (v .>>. 7)
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RL (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RL (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          rlhl `shouldHaveCycles` 4
          let expected = (v .<<. 1) .|. (if cy then 1 else 0)
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RRC (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RRC ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          rrchl `shouldHaveCycles` 4
          let expected = (v .>>. 1) .|. (v .<<. 7)
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "RR (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for RR (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          cy <- CPUM $ testFlag flagCY
          rrhl `shouldHaveCycles` 4
          let expected = (v .>>. 1) .|. (if cy then 0x80 else 0)
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SLA r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for SLA " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          slar r `shouldHaveCycles` 2
          let expected = v .<<. 1
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "SLA (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SLA (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          slahl `shouldHaveCycles` 4
          let expected = v .<<. 1
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "SRA r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for SRA " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          srar r `shouldHaveCycles` 2
          let expected = (v .>>. 1) .|. (v .&. 0x80)
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SRA (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SRA (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          srahl `shouldHaveCycles` 4
          let expected = (v .>>. 1) .|. (v .&. 0x80)
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SRL r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for SRL " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          srlr r `shouldHaveCycles` 2
          let expected = v .>>. 1
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SRL (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SRL (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          srlhl `shouldHaveCycles` 4
          let expected = v .>>. 1
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SWAP r" $ for_ [ (r, v) | r <- allRegisters, v <- [minBound .. maxBound] ] $ \(r, v) ->
    it ("Works for SWAP " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValuesInRegisters [(r, v)]
      $ alteringFlags allFlags
      $ do
          swapr r `shouldHaveCycles` 2
          let expected = (v .>>. 4) .|. (v .<<. 4)
          r `registerShouldBe` expected
          expectFlags allFlags (buildFlags False False False (expected == 0))

  describe "SWAP (HL)" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for SWAP (HL) ; ((HL) = " <> formatHex v <> ")")
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags allFlags
      $ do
          swaphl `shouldHaveCycles` 4
          let expected = (v .>>. 4) .|. (v .<<. 4)
          0xC000 `atAddressShouldBe` expected
          RegHL `registerSSShouldBe` 0xC000
          expectFlags allFlags (buildFlags False False False (expected == 0))

bitOperations :: Spec
bitOperations = do
  describe "BIT b, r"
    $ for_ [ (r, b, 1 .<<. v) | r <- allRegisters, b <- [0 .. 7], v <- [0 .. 7] ]
    $ \(r, b, v) ->
        it ("Works for " <> formatBitR r b v)
          $ withNewCPU
          $ withValuesInRegisters [(r, v)]
          $ alteringFlags (flagH .|. flagN .|. flagZ)
          $ do
              bitr r (fromIntegral b) `shouldHaveCycles` 2
              r `registerShouldBe` v
              expectFlags (flagH .|. flagN .|. flagZ)
                          (buildFlags False True False (not (v `testBit` b)))

  describe "BIT b, (HL)" $ for_ [ (b, 1 .<<. v) | b <- [0 .. 7], v <- [0 .. 7] ] $ \(b, v) ->
    it ("Works for " <> formatBitHL b v)
      $ withNewCPU
      $ withValueAt RegHL 0xC000 v
      $ alteringFlags (flagH .|. flagN .|. flagZ)
      $ do
          bithl (fromIntegral b) `shouldHaveCycles` 3
          0xC000 `atAddressShouldBe` v
          RegHL `registerSSShouldBe` 0xC000
          expectFlags (flagH .|. flagN .|. flagZ)
                      (buildFlags False True False (not (v `testBit` b)))

  describe "SET b, r"
    $ for_ [ (r, b, 1 .<<. v) | r <- allRegisters, b <- [0 .. 7], v <- [0 .. 7] ]
    $ \(r, b, v) ->
        it ("Works for " <> formatSetR r b v) $ withNewCPU $ withValuesInRegisters [(r, v)] $ do
          setr r (fromIntegral b) `shouldHaveCycles` 2
          r `registerShouldBe` (v `setBit` b)

  describe "SET b, (HL)" $ for_ [ (b, 1 .<<. v) | b <- [0 .. 7], v <- [0 .. 7] ] $ \(b, v) ->
    it ("Works for " <> formatSetHL b v) $ withNewCPU $ withValueAt RegHL 0xC000 v $ do
      sethl (fromIntegral b) `shouldHaveCycles` 4
      0xC000 `atAddressShouldBe` (v `setBit` b)
      RegHL `registerSSShouldBe` 0xC000

  describe "RES b, r"
    $ for_ [ (r, b, 1 .<<. v) | r <- allRegisters, b <- [0 .. 7], v <- [0 .. 7] ]
    $ \(r, b, v) ->
        it ("Works for " <> formatResR r b v) $ withNewCPU $ withValuesInRegisters [(r, v)] $ do
          resr r (fromIntegral b) `shouldHaveCycles` 2
          r `registerShouldBe` (v `clearBit` b)

  describe "RES b, (HL)" $ for_ [ (b, 1 .<<. v) | b <- [0 .. 7], v <- [0 .. 7] ] $ \(b, v) ->
    it ("Works for " <> formatResHL b v) $ withNewCPU $ withValueAt RegHL 0xC000 v $ do
      reshl (fromIntegral b) `shouldHaveCycles` 4
      0xC000 `atAddressShouldBe` (v `clearBit` b)
      RegHL `registerSSShouldBe` 0xC000

 where
  formatBitR r b v =
    "BIT " <> show b <> ", " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")"
  formatBitHL b v = "BIT " <> show b <> ", (HL) ; ((HL) = " <> formatHex v <> ")"
  formatSetR r b v =
    "SET " <> show b <> ", " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")"
  formatSetHL b v = "SET " <> show b <> ", (HL) ; ((HL) = " <> formatHex v <> ")"
  formatResR r b v =
    "RES " <> show b <> ", " <> format r <> " ; (" <> format r <> " = " <> formatHex v <> ")"
  formatResHL b v = "RES " <> show b <> ", (HL) ; ((HL) = " <> formatHex v <> ")"

jumps :: Spec
jumps = do
  describe "JP nn" $ it "Works for JP 4232" $ withNewCPU $ alteringPC $ do
    jpnn 0x4232 `shouldHaveCycles` 4
    expectPC 0x4232

  describe "JP (HL)"
    $ it "Works for JP (HL) ; (HL = 4232)"
    $ withNewCPU
    $ withValuesInSSRegisters [(RegHL, 0x4232)]
    $ alteringPC
    $ do
        jphl `shouldHaveCycles` 1
        expectPC 0x4232
        RegHL `registerSSShouldBe` 0x4232

  describe "JP cc, nn"
    $ for_ [ (cc, shouldJump) | cc <- allConditions, shouldJump <- [True, False] ]
    $ \(cc, shouldJump) ->
        it ("Works for JP " <> format cc <> ", 4232 ; when condition is " <> show shouldJump)
          $ withNewCPU
          $ alteringFlags allFlags
          $ if shouldJump
              then alteringPC $ do
                setCondition cc
                notAlteringFlags allFlags $ do
                  jpccnn cc 0x4232 `shouldHaveCycles` 4
                  expectPC 0x4232
              else do
                clearCondition cc
                notAlteringFlags allFlags $ jpccnn cc 0x4232 `shouldHaveCycles` 3

  describe "JR e" $ for_ [-8 .. 8] $ \e ->
    it ("Works for JR " <> formatHex e) $ withNewCPU $ withPC 0x4000 $ do
      jr e `shouldHaveCycles` 3
      expectPC (0x4000 + fromIntegral e)

  describe "JR cc, e"
    $ for_
        [ (cc, shouldJump, e) | cc <- allConditions, shouldJump <- [True, False], e <- [-8 .. 8] ]
    $ \(cc, shouldJump, e) ->
        it ("Works for " <> formatJRcc cc e shouldJump)
          $ withNewCPU
          $ alteringFlags allFlags
          $ if shouldJump
              then withPC 0x4000 $ do
                setCondition cc
                notAlteringFlags allFlags $ do
                  jrcc cc e `shouldHaveCycles` 3
                  expectPC (0x4000 + fromIntegral e)
              else do
                clearCondition cc
                notAlteringFlags allFlags $ jrcc cc e `shouldHaveCycles` 2
 where
  formatJRcc cc e shouldJump =
    "JR " <> format cc <> ", " <> formatHex e <> "; when condition is " <> show shouldJump

callAndReturn :: Spec
callAndReturn = do
  describe "CALL nn"
    $ it "Works for CALL 4232"
    $ withNewCPU
    $ withValuesInSSRegisters [(RegSP, 0xFFF0)]
    $ withPC 0x4001
    $ do
        call 0x4232 `shouldHaveCycles` 6
        expectPC 0x4232
        RegSP `registerSSShouldBe` 0xFFEE
        0xFFEE `atAddressShouldBe` 0x01
        0xFFEF `atAddressShouldBe` 0x40

  describe "CALL cc, nn"
    $ for_ [ (cc, shouldCall) | cc <- allConditions, shouldCall <- [True, False] ]
    $ \(cc, shouldCall) ->
        it ("Works for CALL " <> format cc <> ", 4232 ; with condition " <> show shouldCall)
          $ withNewCPU
          $ withValuesInSSRegisters [(RegSP, 0xFFF0)]
          $ alteringFlags allFlags
          $ if shouldCall
              then withPC 0x4001 $ do
                setCondition cc
                notAlteringFlags allFlags $ do
                  callcc cc 0x4232 `shouldHaveCycles` 6
                  expectPC 0x4232
                  RegSP `registerSSShouldBe` 0xFFEE
                  0xFFEE `atAddressShouldBe` 0x01
                  0xFFEF `atAddressShouldBe` 0x40
              else do
                clearCondition cc
                notAlteringFlags allFlags $ do
                  callcc cc 0x4232 `shouldHaveCycles` 3
                  RegSP `registerSSShouldBe` 0xFFF0

  describe "RET"
    $ it "Works for RET ; (SP) = 4232"
    $ withNewCPU
    $ withValue16At RegSP 0xFFEE 0x4232
    $ withPC 0x4001
    $ do
        ret `shouldHaveCycles` 4
        expectPC 0x4232
        RegSP `registerSSShouldBe` 0xFFF0

  describe "RETI" $ for_ [True, False] $ \ime ->
    it ("Works for RETI ; (SP) = 4232, IME = " <> show ime)
      $ withNewCPU
      $ withValue16At RegSP 0xFFEE 0x4232
      $ withPC 0x4001
      $ withIME ime
      $ do
          reti `shouldHaveCycles` 4
          expectPC 0x4232
          expectIME True
          RegSP `registerSSShouldBe` 0xFFF0

  describe "RET cc"
    $ for_ [ (cc, shouldRet) | cc <- allConditions, shouldRet <- [True, False] ]
    $ \(cc, shouldRet) ->
        it ("Works for RET " <> format cc <> "; (SP) = 4232, condition is " <> show shouldRet)
          $ withNewCPU
          $ withValue16At RegSP 0xFFEE 0x4232
          $ alteringFlags allFlags
          $ if shouldRet
              then withPC 0x4001 $ do
                setCondition cc
                notAlteringFlags allFlags $ do
                  retcc cc `shouldHaveCycles` 5
                  expectPC 0x4232
                  RegSP `registerSSShouldBe` 0xFFF0
              else do
                clearCondition cc
                notAlteringFlags allFlags $ do
                  retcc cc `shouldHaveCycles` 2
                  RegSP `registerSSShouldBe` 0xFFEE

  describe "RST t" $ for_ [0 .. 7] $ \t ->
    it ("Works for RST " <> show t)
      $ withNewCPU
      $ withValuesInSSRegisters [(RegSP, 0xFFF0)]
      $ withPC 0x4001
      $ do
          rst t `shouldHaveCycles` 4
          expectPC (fromIntegral t * 8)
          RegSP `registerSSShouldBe` 0xFFEE
          0xFFEE `atAddressShouldBe` 0x01
          0xFFEF `atAddressShouldBe` 0x40

--  stop    :: m (ExecuteResult m)

miscellaneous :: Spec
miscellaneous = do
  describe "CPL" $ for_ [minBound .. maxBound] $ \v ->
    it ("Works for CPL ; A = " <> formatHex v)
      $ withNewCPU
      $ withValuesInRegisters [(RegA, v)]
      $ alteringFlags (flagH .|. flagN)
      $ do
          cpl `shouldHaveCycles` 1
          RegA `registerShouldBe` complement v
          expectFlags (flagH .|. flagN) (flagH .|. flagN)

  describe "NOP" $ it "Does nothing" $ withNewCPU $ nop `shouldHaveCycles` 1

  describe "CCF" $ for_ [True, False] $ \cf0 ->
    it ("Works for CCF ; CY = " <> show cf0)
      $ withNewCPU
      $ alteringFlags (flagCY .|. flagH .|. flagN)
      $ do
          CPUM $ setFlagsMask flagCY (if cf0 then flagCY else 0)
          ccf `shouldHaveCycles` 1
          expectFlags (flagCY .|. flagH .|. flagN) (buildFlags (not cf0) False False False)

  describe "SCF" $ it "Works for SCF" $ withNewCPU $ alteringFlags (flagCY .|. flagH .|. flagN) $ do
    scf `shouldHaveCycles` 1
    expectFlags (flagCY .|. flagH .|. flagN) (buildFlags True False False False)

  describe "DI" $ for_ [True, False] $ \ime0 ->
    it ("Works for DI ; IME = " <> show ime0) $ withNewCPU $ withIME ime0 $ do
      di `shouldHaveCycles` 1
      expectIME False

  describe "EI" $ for_ [True, False] $ \ime0 ->
    it ("Works for EI ; IME = " <> show ime0) $ withNewCPU $ withIME ime0 $ do
      ei `shouldHaveCycles` 1
      expectIME True

  describe "HALT" $ it "Enters salt mode" $ withNewCPU $ alteringMode $ do
    halt `shouldHaveCycles` 1
    expectMode ModeHalt

  describe "STOP" $ do
    it "Enters stop mode" $ withNewCPU $ alteringMode $ do
      stop `shouldHaveCycles` 1
      expectMode ModeStop
    it "Switches speed mode" $ withNewCPU $ alteringCPUCycleClocks $ do
      KEY1 `atAddressShouldBe` 0
      CPUM $ writeByte KEY1 1
      KEY1 `atAddressShouldBe` 1
      stop `shouldHaveCycles` 1
      KEY1 `atAddressShouldBe` 0x80
      expectCPUCycleClocks 2
      CPUM $ writeByte KEY1 1
      KEY1 `atAddressShouldBe` 0x81
      stop `shouldHaveCycles` 1
      KEY1 `atAddressShouldBe` 0x00
      expectCPUCycleClocks 4

bcd :: Spec
bcd = do
  describe "DAA after addition" $ traverse_ testAddition allBCDCombos
  describe "DAA after subtraction" $ traverse_ testSubtraction allBCDCombos
 where
  allBCDCombos = [ (a, b) | a <- [0 .. 99], b <- [0 .. 99] ]

  toBCD :: Word8 -> Word16
  toBCD x =
    let (r0, d0) = x `divMod` 10
        (d2, d1) = r0 `divMod` 10
    in  (fromIntegral d2 `shiftL` 8) .|. (fromIntegral d1 `shiftL` 4) .|. fromIntegral d0
  toBCD8 = fromIntegral . toBCD

  testSubtraction (a, b) =
    it ("works for " ++ show a ++ " - " ++ show b)
      $ withNewCPU
      $ alteringFlags allFlags
      $ withValuesInRegisters [(RegA, toBCD8 a)]
      $ do
          subn (toBCD8 b) `shouldHaveCycles` 2
          daa `shouldHaveCycles` 1
          cy <- CPUM $ testFlag flagCY
          a' <- CPUM $ readR8 RegA
          let result   = fromIntegral a' + if cy then 0x0100 else 0
          let expected = if a < b then toBCD (a - b + 100) + 0x0100 else toBCD (a - b)
          liftIO (result `shouldBe` expected)

  testAddition (a, b) =
    it ("works for " ++ show a ++ " + " ++ show b)
      $ withNewCPU
      $ alteringFlags allFlags
      $ withValuesInRegisters [(RegA, toBCD8 a)]
      $ do
          addn (toBCD8 b) `shouldHaveCycles` 2
          daa `shouldHaveCycles` 1
          cy <- CPUM $ testFlag flagCY
          a' <- CPUM $ readR8 RegA
          let result = fromIntegral a' + if cy then 0x0100 else 0
          liftIO (result `shouldBe` toBCD (a + b))

data InterruptBehavior = Trigger | Ignore | Wakeup deriving (Eq, Show)

interrupts :: Spec
interrupts = do
  describe "Interrupt triggering"
    $ for_
        [ (vector, pending, enabled, ime, mode)
        | vector  <- [0 .. 4]
        , pending <- [True, False]
        , enabled <- [True, False]
        , ime     <- [True, False]
        , mode    <- [minBound .. maxBound]
        ]
    $ \(vector, pending, enabled, ime, mode) ->
        it (triggerMessage vector pending enabled ime mode)
          $ withNewCPU
          $ withIME ime
          $ withMode mode
          $ withValuesInSSRegisters [(RegSP, 0xFFF0)]
          $ withPC 0x4001
          $ do
              let behavior | pending && enabled && ime = Trigger
                           | mode /= ModeNormal && pending && enabled = Wakeup
                           | otherwise                 = Ignore

              CPUM $ writeByte IF (if pending then bit vector else 0)
              CPUM $ writeByte IE (if enabled then bit vector else 0)
              cycles <- CPUM cpuStep
              liftIO $ cycles `shouldBe` case behavior of
                Trigger -> 7
                Wakeup  -> 1
                Ignore  -> if mode == ModeNormal then 1 else 8

              case behavior of
                Trigger -> do
                  RegSP `registerSSShouldBe` 0xFFEE
                  0xFFEE `atAddressShouldBe` 0x01
                  0xFFEF `atAddressShouldBe` 0x40
                  IF `atAddressShouldBe` 0
                  IE `atAddressShouldBe` (bit vector)
                  expectPC (fromIntegral vector * 8 + 0x40)
                  expectMode ModeNormal
                  expectIME False
                Wakeup -> do
                  RegSP `registerSSShouldBe` 0xFFF0
                  IF `atAddressShouldBe` (if pending then bit vector else 0)
                  IE `atAddressShouldBe` (if enabled then bit vector else 0)
                  expectPC 0x4002
                  expectMode ModeNormal
                  expectIME ime
                Ignore -> do
                  RegSP `registerSSShouldBe` 0xFFF0
                  IF `atAddressShouldBe` (if pending then bit vector else 0)
                  IE `atAddressShouldBe` (if enabled then bit vector else 0)
                  expectPC (if mode == ModeNormal then 0x4002 else 0x4001)
                  expectMode mode
                  expectIME ime

  describe "Invalid interrupts" $ for_ [5, 6, 7] $ \vector ->
    it ("Does not trigger on invalid interrupt " <> show vector)
      $ withNewCPU
      $ withMode ModeNormal
      $ withIME True
      $ withPC 0x4001
      $ do
          CPUM $ do
            writeByte IF (bit vector)
            writeByte IE (bit vector)
            cycles <- cpuStep
            liftIO (cycles `shouldBe` 1)
          expectMode ModeNormal
          expectIME True
          expectPC 0x4002

  describe "Interrupt priority" $ for_ [ (l, r) | l <- [0 .. 4], r <- [l + 1 .. 4] ] $ \(l, r) ->
    it ("Triggers " <> show l <> " when " <> show l <> " and " <> show r <> " are both pending")
      $ withNewCPU
      $ withValuesInSSRegisters [(RegSP, 0xFFF0)]
      $ withMode ModeNormal
      $ withIME True
      $ withPC 0x4001
      $ do
          CPUM $ do
            writeByte IF (bit l .|. bit r)
            writeByte IE (bit l .|. bit r)
            cycles <- cpuStep
            liftIO (cycles `shouldBe` 7)
          RegSP `registerSSShouldBe` 0xFFEE
          0xFFEE `atAddressShouldBe` 0x01
          0xFFEF `atAddressShouldBe` 0x40
          IF `atAddressShouldBe` (bit r)
          IE `atAddressShouldBe` (bit l .|. bit r)
          expectPC (fromIntegral l * 8 + 0x40)
          expectMode ModeNormal
          expectIME False

 where
  triggerMessage vector True True True mode =
    "Triggers " <> show vector <> " when enabled, pending, IME is set, and in mode " <> show mode
  triggerMessage vector pending enabled ime mode =
    "Does not trigger "
      <> show vector
      <> " when "
      <> (if enabled then "enabled" else "not enabled")
      <> (if pending then ", pending" else ", not pending")
      <> (if ime then ", IME is set" else ", IME is not set")
      <> ", and in mode "
      <> show mode
