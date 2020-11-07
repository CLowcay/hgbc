{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.CPUSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bits
import Data.Foldable
import Data.IORef
import qualified Data.Vector.Storable as VS
import Data.Word
import qualified Machine.GBC.Bus as Bus
import qualified Machine.GBC.CPU as CPU
import Machine.GBC.CPU.ISA
import qualified Machine.GBC.Color as Color
import Machine.GBC.Graphics.VRAM
import Machine.GBC.MBC
import qualified Machine.GBC.Memory as Memory
import Machine.GBC.Mode
import Machine.GBC.Primitive
import Machine.GBC.ROM
import Machine.GBC.Registers
import Machine.GBC.Util
import Test.Hspec

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

romSizeInBytes = 32 * 1024 * 1024

blankROM = VS.replicate romSizeInBytes 0

blankHeader :: Header
blankHeader =
  Header
    { startAddress = 0,
      nintendoCharacterData = "",
      gameTitle = "",
      gameCode = "",
      cgbSupport = CGBCompatible,
      makerCode = "",
      sgbSupport = GBOnly,
      cartridgeType = CartridgeType Nothing False False,
      romSize = romSizeInBytes,
      externalRAM = 0,
      destination = Overseas,
      oldLicenseCode = 0,
      maskROMVersion = 0
    }

data CPUTestState = CPUTestState
  { testCPU :: !CPU.State,
    testMemory :: !Memory.State,
    extraCycles :: !(IORef Int)
  }

instance CPU.Has CPUTestState where
  forState = testCPU

instance Memory.Has CPUTestState where
  forState = testMemory

instance Bus.Has CPUTestState where
  read address = do
    extraCyclesRef <- asks extraCycles
    liftIO $ modifyIORef' extraCyclesRef (+ 1)
    Memory.readByte address
  write address value = do
    extraCyclesRef <- asks extraCycles
    liftIO $ modifyIORef' extraCyclesRef (+ 1)
    Memory.writeByte address value
  delay = do
    extraCyclesRef <- asks extraCycles
    liftIO $ modifyIORef' extraCyclesRef (+ 1)
  delayClocks = error "CPU should not call delayClocks"

withNewCPU :: CPU.M CPUTestState () -> IO ()
withNewCPU computation = mdo
  vram <- initVRAM (Color.correction Color.NoCorrection)
  portIF <- newPort 0x00 0x1F alwaysUpdate
  portIE <- newPort 0x00 0xFF alwaysUpdate
  mbc <- nullMBC
  modeRef <- newIORef CGB
  mem <-
    Memory.init
      Nothing
      blankROM
      blankHeader
      mbc
      vram
      ((IF, portIF) : CPU.ports cpu)
      portIE
      modeRef
  extraCycles <- newIORef 0
  cpu <- CPU.init portIF portIE modeRef
  void $ runReaderT checkingFlags $ CPUTestState cpu mem extraCycles
  where
    checkingFlags = for_ [0 .. 15] $ \flags -> do
      CPU.reset
      let flags0 = flags .<<. 4
      CPU.writeF flags0
      computationWithVerification
      flags1 <- CPU.readF
      liftIO $ flags1 `shouldBe` flags0

    computationWithVerification = do
      state <- ask
      liftIO $ writeIORef (extraCycles state) 0

      registers0 <- CPU.getRegisterFile
      mode0 <- CPU.getMode
      clocks0 <- CPU.getCycleClocks

      CPU.run computation

      registers1 <- CPU.getRegisterFile
      mode1 <- CPU.getMode
      clocks1 <- CPU.getCycleClocks

      registersConsistent
      liftIO $ do
        registers1 `shouldBe` registers0
        mode0 `shouldBe` mode1
        clocks0 `shouldBe` clocks1

    registersConsistent = do
      bc <- CPU.readR16 RegBC
      de <- CPU.readR16 RegDE
      hl <- CPU.readR16 RegHL
      b <- CPU.readR8 RegB
      c <- CPU.readR8 RegC
      d <- CPU.readR8 RegD
      e <- CPU.readR8 RegE
      h <- CPU.readR8 RegH
      l <- CPU.readR8 RegL
      f <- CPU.readF
      liftIO $ do
        f .&. 0x0F `shouldBe` 0
        fromIntegral (bc .>>. 8) `shouldBe` b
        fromIntegral (de .>>. 8) `shouldBe` d
        fromIntegral (hl .>>. 8) `shouldBe` h
        fromIntegral (bc .&. 0x00FF) `shouldBe` c
        fromIntegral (de .&. 0x00FF) `shouldBe` e
        fromIntegral (hl .&. 0x00FF) `shouldBe` l

alteringRegisters :: [Register8] -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringRegisters rs computation = do
  registers <- CPU.M CPU.getRegisterFile
  computation
  CPU.M $
    for_ rs $ \case
      RegA -> CPU.writeR8 RegA (CPU.regA registers)
      RegB -> CPU.writeR8 RegB (CPU.regB registers)
      RegC -> CPU.writeR8 RegC (CPU.regC registers)
      RegD -> CPU.writeR8 RegD (CPU.regD registers)
      RegE -> CPU.writeR8 RegE (CPU.regE registers)
      RegH -> CPU.writeR8 RegH (CPU.regH registers)
      RegL -> CPU.writeR8 RegL (CPU.regL registers)

alteringSSRegisters :: [Register16] -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringSSRegisters rs computation = do
  registers <- CPU.M CPU.getRegisterFile
  computation
  CPU.M $
    for_ rs $ \case
      RegBC -> do
        CPU.writeR8 RegB (CPU.regB registers)
        CPU.writeR8 RegC (CPU.regC registers)
      RegDE -> do
        CPU.writeR8 RegD (CPU.regD registers)
        CPU.writeR8 RegE (CPU.regE registers)
      RegHL -> do
        CPU.writeR8 RegH (CPU.regH registers)
        CPU.writeR8 RegL (CPU.regL registers)
      RegSP -> CPU.writeR16 RegSP (CPU.regSP registers)

alteringQQRegisters :: [RegisterPushPop] -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringQQRegisters rs computation = do
  registers <- CPU.M CPU.getRegisterFile
  computation
  CPU.M $
    for_ rs $ \case
      PushPopBC -> do
        CPU.writeR8 RegB (CPU.regB registers)
        CPU.writeR8 RegC (CPU.regC registers)
      PushPopDE -> do
        CPU.writeR8 RegD (CPU.regD registers)
        CPU.writeR8 RegE (CPU.regE registers)
      PushPopHL -> do
        CPU.writeR8 RegH (CPU.regH registers)
        CPU.writeR8 RegL (CPU.regL registers)
      PushPopAF -> do
        CPU.writeR8 RegA (CPU.regA registers)
        CPU.writeF (CPU.regF registers)

alteringSP :: CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringSP computation = do
  sp <- CPU.M $ CPU.readR16 RegSP
  computation
  CPU.M $ CPU.writeR16 RegSP sp

alteringPC :: CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringPC computation = do
  pc <- CPU.M CPU.readPC
  computation
  CPU.M $ CPU.writePC pc

alteringMode :: CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringMode computation = do
  mode0 <- CPU.M CPU.getMode
  computation
  CPU.M $ CPU.setMode mode0

alteringCPUCycleClocks :: CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringCPUCycleClocks computation = do
  clocks <- CPU.M CPU.getCycleClocks
  computation
  CPU.M $ CPU.setCycleClocks clocks

alteringFlags :: Word8 -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
alteringFlags mask computation = do
  flags0 <- CPU.M CPU.readF
  computation
  flags1 <- CPU.M CPU.readF
  CPU.M $ CPU.writeF ((flags0 .&. mask) .|. (flags1 .&. complement mask))

notAlteringFlags :: Word8 -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
notAlteringFlags mask computation = do
  flags0 <- CPU.M CPU.readF
  computation
  flags1 <- CPU.M CPU.readF
  liftIO ((flags1 .&. mask) `shouldBe` (flags0 .&. mask))

withPC :: Word16 -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withPC address computation = do
  pc0 <- CPU.M CPU.readPC
  CPU.M $ CPU.writePC address
  computation
  CPU.M $ CPU.writePC pc0

withIME :: Bool -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withIME ime computation = do
  ime0 <- CPU.M CPU.testIME
  CPU.M $ if ime then CPU.setIME else CPU.clearIME
  computation
  CPU.M $ if ime0 then CPU.setIME else CPU.clearIME

withMode :: CPU.Mode -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withMode mode computation = do
  mode0 <- CPU.M CPU.getMode
  CPU.M $ CPU.setMode mode
  computation
  CPU.M $ CPU.setMode mode0

withValueAt :: Register16 -> Word16 -> Word8 -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withValueAt ss address value computation = alteringSSRegisters [ss] $ do
  CPU.M $ do
    CPU.writeR16 ss address
    Memory.writeByte address value
  computation

withValue16At :: Register16 -> Word16 -> Word16 -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withValue16At ss address value computation = alteringSSRegisters [ss] $ do
  CPU.M $ do
    CPU.writeR16 ss address
    Memory.writeByte address (fromIntegral (value .&. 0xFF))
    Memory.writeByte (address + 1) (fromIntegral (value .>>. 8))
  computation

withValueAtC :: Word8 -> Word8 -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withValueAtC address value computation = alteringRegisters [RegC] $ do
  CPU.M $ do
    CPU.writeR8 RegC address
    Memory.writeByte (0xFF00 + fromIntegral address) value
  computation

withValuesInRegisters :: [(Register8, Word8)] -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withValuesInRegisters rvs computation = alteringRegisters (fst <$> rvs) $ do
  for_ rvs $ \(r, v) -> CPU.M $ CPU.writeR8 r v
  computation

withValuesInSSRegisters :: [(Register16, Word16)] -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withValuesInSSRegisters rvs computation = alteringSSRegisters (fst <$> rvs) $ do
  for_ rvs $ \(r, v) -> CPU.M $ CPU.writeR16 r v
  computation

withValuesInQQRegisters ::
  [(RegisterPushPop, Word16)] -> CPU.M CPUTestState () -> CPU.M CPUTestState ()
withValuesInQQRegisters rvs computation = alteringQQRegisters (fst <$> rvs) $ do
  for_ rvs $ \(r, v) -> CPU.M $ CPU.writeR16pp r v
  computation

setCondition :: ConditionCode -> CPU.M CPUTestState ()
setCondition CondC = CPU.M $ CPU.setFlagsMask CPU.flagCY CPU.flagCY
setCondition CondNC = CPU.M $ CPU.setFlagsMask CPU.flagCY 0
setCondition CondZ = CPU.M $ CPU.setFlagsMask CPU.flagZ CPU.flagZ
setCondition CondNZ = CPU.M $ CPU.setFlagsMask CPU.flagZ 0

clearCondition :: ConditionCode -> CPU.M CPUTestState ()
clearCondition CondC = CPU.M $ CPU.setFlagsMask CPU.flagCY 0
clearCondition CondNC = CPU.M $ CPU.setFlagsMask CPU.flagCY CPU.flagCY
clearCondition CondZ = CPU.M $ CPU.setFlagsMask CPU.flagZ 0
clearCondition CondNZ = CPU.M $ CPU.setFlagsMask CPU.flagZ CPU.flagZ

registerShouldBe :: Register8 -> Word8 -> CPU.M CPUTestState ()
registerShouldBe r expected = do
  v <- CPU.M $ CPU.readR8 r
  liftIO (v `shouldBe` expected)

register16ShouldBe :: Register16 -> Word16 -> CPU.M CPUTestState ()
register16ShouldBe r expected = do
  v <- CPU.M $ CPU.readR16 r
  liftIO (v `shouldBe` expected)

registerPPShouldBe :: RegisterPushPop -> Word16 -> CPU.M CPUTestState ()
registerPPShouldBe r expected = do
  v <- CPU.M $ CPU.readR16pp r
  liftIO (v `shouldBe` expected)

atAddressShouldBe :: Word16 -> Word8 -> CPU.M CPUTestState ()
atAddressShouldBe address value = do
  v <- CPU.M $ Memory.readByte address
  liftIO (v `shouldBe` value)

expectFlags :: Word8 -> Word8 -> CPU.M CPUTestState ()
expectFlags mask expected = do
  flags <- CPU.M CPU.readF
  liftIO ((flags .&. mask) `shouldBe` (expected .&. mask))

expectPC :: Word16 -> CPU.M CPUTestState ()
expectPC expected = do
  pc <- CPU.M CPU.readPC
  liftIO (pc `shouldBe` expected)

expectMode :: CPU.Mode -> CPU.M CPUTestState ()
expectMode expected = do
  mode <- CPU.M CPU.getMode
  liftIO (mode `shouldBe` expected)

expectCPUCycleClocks :: Int -> CPU.M CPUTestState ()
expectCPUCycleClocks expected = do
  clocks <- CPU.M CPU.getCycleClocks
  liftIO (clocks `shouldBe` expected)

expectIME :: Bool -> CPU.M CPUTestState ()
expectIME expected = do
  ime <- CPU.M CPU.testIME
  liftIO (ime `shouldBe` expected)

expectExtraCycles :: Int -> CPU.M CPUTestState ()
expectExtraCycles expected = CPU.M $ do
  CPUTestState _ _ cyclesRef <- ask
  liftIO $ do
    cycles <- readIORef cyclesRef
    cycles `shouldBe` expected

buildFlags :: Bool -> Bool -> Bool -> Bool -> Word8
buildFlags cy h n z =
  (if cy then CPU.flagCY else 0)
    .|. (if h then CPU.flagH else 0)
    .|. (if n then CPU.flagN else 0)
    .|. (if z then CPU.flagZ else 0)

allFlags :: Word8
allFlags = 0xF0

allConditions :: [ConditionCode]
allConditions = [minBound .. maxBound]

allRegisters :: [Register8]
allRegisters = [minBound .. maxBound]

allRegisters16 :: [Register16]
allRegisters16 = [minBound .. maxBound]

allRegistersPP :: [RegisterPushPop]
allRegistersPP = [minBound .. maxBound]

-- | Check if there is a carry into the specified bit when performing a binary
-- operation on two values.
carryIntoBit ::
  (Integral a, Integral b, Bits a, Bits b) =>
  Int ->
  a ->
  b ->
  (Word32 -> Word32 -> Word32) ->
  Bool
carryIntoBit i a b op =
  ((fromIntegral a `clearBit` i) `op` (fromIntegral b `clearBit` i)) `testBit` i

loads :: Spec
loads = do
  describe "LD r, r" $
    for_ [(r, r') | r <- allRegisters, r' <- allRegisters] $ \(r, r') ->
      it ("Works for LD " <> show r <> ", " <> show r') $
        withNewCPU $
          alteringRegisters [r] $
            withValuesInRegisters [(r', 42)] $
              do
                ldrr r r'
                expectExtraCycles 0
                r `registerShouldBe` 42

  describe "LD r, n" $
    for_ allRegisters $ \r ->
      it ("Works for LD " <> show r <> ", 42") $
        withNewCPU $
          alteringRegisters [r] $ do
            ldrn r 42
            expectExtraCycles 0
            r `registerShouldBe` 42

  describe "LD r, (HL)" $
    for_ allRegisters $ \r ->
      it ("Works for LD " <> show r <> ", (HL)") $
        withNewCPU $
          alteringRegisters [r] $
            withValueAt RegHL 0xC000 42 $
              do
                ldrHL r
                expectExtraCycles 1
                r `registerShouldBe` 42
                RegH `registerShouldBe` (if r == RegH then 42 else 0xC0)
                RegL `registerShouldBe` (if r == RegL then 42 else 0)

  describe "LD (HL), r" $
    for_ allRegisters $ \r ->
      it ("Works for LD (HL), " <> show r) $
        withNewCPU $
          withValuesInRegisters [(r, 42)] $
            withValueAt RegHL 0xC0D0 32 $
              do
                ldHLr r
                expectExtraCycles 1
                let expected = case r of
                      RegH -> 0xC0
                      RegL -> 0xD0
                      _ -> 42
                r `registerShouldBe` expected
                RegH `registerShouldBe` 0xC0
                RegL `registerShouldBe` 0xD0
                0xC0D0 `atAddressShouldBe` expected

  describe "LD (HL), n" $
    it "Works for LD (HL), 42" $
      withNewCPU $
        withValueAt RegHL 0xC000 32 $ do
          ldHLn 42
          expectExtraCycles 1
          0xC000 `atAddressShouldBe` 42
          RegH `registerShouldBe` 0xC0
          RegL `registerShouldBe` 0

  describe "LD A, (BC)" $
    it "Works for LD A, (BC)" $
      withNewCPU $
        alteringRegisters [RegA] $
          withValueAt RegBC 0xC000 42 $
            do
              ldaBC
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              0xC000 `atAddressShouldBe` 42
              RegB `registerShouldBe` 0xC0
              RegC `registerShouldBe` 0

  describe "LD A, (DE)" $
    it "Works for LD A, (DE)" $
      withNewCPU $
        alteringRegisters [RegA] $
          withValueAt RegDE 0xC000 42 $
            do
              ldaDE
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              0xC000 `atAddressShouldBe` 42
              RegD `registerShouldBe` 0xC0
              RegE `registerShouldBe` 0

  describe "LD A, (C)" $
    it "Works for LD A, (C)" $
      withNewCPU $
        alteringRegisters [RegA] $
          withValueAtC 0x80 42 $
            do
              ldaC
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              0xFF80 `atAddressShouldBe` 42
              RegC `registerShouldBe` 0x80

  describe "LD (C), A" $
    it "Works for LD (C), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          withValueAtC 0x80 32 $
            do
              ldCa
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegC `registerShouldBe` 0x80
              0xFF80 `atAddressShouldBe` 42

  describe "LD A, (n)" $
    it "Works for LD A, (80)" $
      withNewCPU $
        alteringRegisters [RegA] $ do
          CPU.M $ Memory.writeByte 0xFF80 42
          ldan 0x80
          expectExtraCycles 1
          RegA `registerShouldBe` 42
          0xFF80 `atAddressShouldBe` 42

  describe "LD (n), A" $
    it "Works for LD (80), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          do
            CPU.M $ Memory.writeByte 0xFF80 32
            ldna 0x80
            expectExtraCycles 1
            RegA `registerShouldBe` 42
            0xFF80 `atAddressShouldBe` 42

  describe "LD A, (nn)" $
    it "Works for LD A, (C000)" $
      withNewCPU $
        alteringRegisters [RegA] $ do
          CPU.M $ Memory.writeByte 0xC000 42
          ldann 0xC000
          expectExtraCycles 1
          RegA `registerShouldBe` 42
          0xC000 `atAddressShouldBe` 42

  describe "LD (nn), A" $
    it "Works for LD (C000), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          do
            CPU.M $ Memory.writeByte 0xC000 32
            ldnna 0xC000
            expectExtraCycles 1
            RegA `registerShouldBe` 42
            0xC000 `atAddressShouldBe` 42

  describe "LD A, (HLI)" $
    it "Works for LD A, (HLI)" $
      withNewCPU $
        alteringRegisters [RegA] $
          withValueAt RegHL 0xC002 42 $
            do
              ldaHLI
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegH `registerShouldBe` 0xC0
              RegL `registerShouldBe` 0x03

  describe "LD A, (HLD)" $
    it "Works for LD A, (HLD)" $
      withNewCPU $
        alteringRegisters [RegA] $
          withValueAt RegHL 0xC002 42 $
            do
              ldaHLD
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegH `registerShouldBe` 0xC0
              RegL `registerShouldBe` 0x01

  describe "LD (BC), A" $
    it "Works for LD (BC), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          withValueAt RegBC 0xC000 32 $
            do
              ldBCa
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegB `registerShouldBe` 0xC0
              RegC `registerShouldBe` 0x00
              0xC000 `atAddressShouldBe` 42

  describe "LD (DE), A" $
    it "Works for LD (DE), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          withValueAt RegDE 0xC000 32 $
            do
              ldDEa
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegD `registerShouldBe` 0xC0
              RegE `registerShouldBe` 0x00
              0xC000 `atAddressShouldBe` 42

  describe "LD (HLI), A" $
    it "Works for LD (HLI), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          withValueAt RegHL 0xC002 32 $
            do
              ldHLIa
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegH `registerShouldBe` 0xC0
              RegL `registerShouldBe` 0x03
              0xC002 `atAddressShouldBe` 42

  describe "LD (HLD), A" $
    it "Works for LD (HLD), A" $
      withNewCPU $
        withValuesInRegisters [(RegA, 42)] $
          withValueAt RegHL 0xC002 32 $
            do
              ldHLDa
              expectExtraCycles 1
              RegA `registerShouldBe` 42
              RegH `registerShouldBe` 0xC0
              RegL `registerShouldBe` 0x01
              0xC002 `atAddressShouldBe` 42

  describe "LD dd, nn" $
    for_ allRegisters16 $ \ss ->
      it ("Works for LD " <> show ss <> ", 4243") $
        withNewCPU $
          alteringSSRegisters [ss] $ do
            ldddnn ss 0x4232
            expectExtraCycles 0
            ss `register16ShouldBe` 0x4232

  describe "LD SP, HL" $
    it "Works for LD SP, HL" $
      withNewCPU $
        alteringSP $
          withValuesInRegisters [(RegH, 0x42), (RegL, 0x32)] $
            do
              ldSPHL
              expectExtraCycles 1
              RegSP `register16ShouldBe` 0x4232
              RegHL `register16ShouldBe` 0x4232

  describe "PUSH qq" $
    for_ allRegistersPP $ \qq ->
      it ("Works for PUSH " <> show qq) $
        withNewCPU $
          withValuesInQQRegisters [(qq, 0x4232)] $
            withValue16At RegSP 0xFFF0 0x2221 $
              do
                push qq
                expectExtraCycles 3
                RegSP `register16ShouldBe` 0xFFEE
                0xFFEE `atAddressShouldBe` (if qq == PushPopAF then 0x30 else 0x32)
                0xFFEF `atAddressShouldBe` 0x42

  describe "POP qq" $
    for_ allRegistersPP $ \qq ->
      it ("Works for POP " <> show qq) $
        withNewCPU $
          alteringQQRegisters [qq] $
            withValue16At RegSP 0xFFEE 0x4232 $
              do
                pop qq
                expectExtraCycles 2
                RegSP `register16ShouldBe` 0xFFF0
                qq `registerPPShouldBe` (if qq == PushPopAF then 0x4230 else 0x4232)
                0xFFEE `atAddressShouldBe` 0x32
                0xFFEF `atAddressShouldBe` 0x42

  describe "LDHL SP, e" $
    for_ [(sp, e) | sp <- [0xF8FF, 0x0012], e <- [-32 .. 32]] $ \(sp, e) ->
      it ("Works for LDHL SP, " <> formatHex e <> " ; (SP = " <> formatHex sp <> ")") $
        withNewCPU $
          withValuesInSSRegisters [(RegSP, sp)] $
            alteringSSRegisters [RegHL] $
              alteringFlags allFlags $
                do
                  ldhl e
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` (sp + fromIntegral e)
                  let carry = carryIntoBit 8 sp e (+)
                  let carryH = carryIntoBit 4 sp e (+)
                  expectFlags
                    allFlags
                    ((if carry then CPU.flagCY else 0) .|. (if carryH then CPU.flagH else 0))

  describe "LD (nn), SP" $
    it "Works for LD (C000), SP" $
      withNewCPU $
        withValuesInSSRegisters [(RegSP, 0xFFF0)] $
          do
            CPU.M $ do
              Memory.writeByte 0xC000 01
              Memory.writeByte 0xC001 01
            ldnnSP 0xC000
            expectExtraCycles 2
            RegSP `register16ShouldBe` 0xFFF0
            0xC000 `atAddressShouldBe` 0xF0
            0xC001 `atAddressShouldBe` 0xFF

arithmetic8 :: Spec
arithmetic8 = do
  describe "ADD A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for ADD A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  addr r
                  expectExtraCycles 0
                  let a = if r == RegA then v else 0x11
                  verifyArithmetic8 a v (+) False

  describe "ADD A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for ADD A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                addn v
                expectExtraCycles 0
                verifyArithmetic8 0x11 v (+) False

  describe "ADD A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for ADD A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  addhl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  verifyArithmetic8 0x11 v (+) False

  describe "ADC A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for ADC A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  cy <- CPU.M $ CPU.testFlag CPU.flagCY
                  adcr r
                  expectExtraCycles 0
                  let a = if r == RegA then v else 0x11
                  verifyArithmetic8 a v (\x y -> x + y + (if cy then 1 else 0)) False

  describe "ADC A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for ADC A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                adcn v
                expectExtraCycles 0
                verifyArithmetic8 0x11 v (\x y -> x + y + (if cy then 1 else 0)) False

  describe "ADC A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for ADC A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  cy <- CPU.M $ CPU.testFlag CPU.flagCY
                  adchl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  verifyArithmetic8 0x11 v (\x y -> x + y + (if cy then 1 else 0)) False

  describe "SUB A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for SUB A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  subr r
                  expectExtraCycles 0
                  let a = if r == RegA then v else 0x11
                  verifyArithmetic8 a v (-) True

  describe "SUB A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SUB A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                subn v
                expectExtraCycles 0
                verifyArithmetic8 0x11 v (-) True

  describe "SUB A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SUB A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  subhl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  verifyArithmetic8 0x11 v (-) True

  describe "SBC A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for SBC A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  cy <- CPU.M $ CPU.testFlag CPU.flagCY
                  sbcr r
                  expectExtraCycles 0
                  let a = if r == RegA then v else 0x11
                  verifyArithmetic8 a v (\x y -> x - y - (if cy then 1 else 0)) True

  describe "SBC A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SBC A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                sbcn v
                expectExtraCycles 0
                verifyArithmetic8 0x11 v (\x y -> x - y - (if cy then 1 else 0)) True

  describe "SBC A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SBC A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  cy <- CPU.M $ CPU.testFlag CPU.flagCY
                  sbchl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  verifyArithmetic8 0x11 v (\x y -> x - y - (if cy then 1 else 0)) True

  describe "AND A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for AND A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  andr r
                  expectExtraCycles 0
                  let expected = v .&. if r == RegA then v else 0x11
                  RegA `registerShouldBe` expected
                  expectFlags allFlags (CPU.flagH .|. (if expected == 0 then CPU.flagZ else 0))

  describe "AND A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for AND A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                andn v
                expectExtraCycles 0
                let expected = v .&. 0x11
                RegA `registerShouldBe` expected
                expectFlags allFlags (CPU.flagH .|. (if expected == 0 then CPU.flagZ else 0))

  describe "AND A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for AND A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  andhl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  let expected = v .&. 0x11
                  expectFlags allFlags (CPU.flagH .|. (if expected == 0 then CPU.flagZ else 0))

  describe "OR A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for OR A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  orr r
                  expectExtraCycles 0
                  let expected = v .|. if r == RegA then v else 0x11
                  RegA `registerShouldBe` expected
                  expectFlags allFlags (if expected == 0 then CPU.flagZ else 0)

  describe "OR A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for OR A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                orn v
                expectExtraCycles 0
                let expected = v .|. 0x11
                RegA `registerShouldBe` expected
                expectFlags allFlags (if expected == 0 then CPU.flagZ else 0)

  describe "OR A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for OR A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  orhl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  let expected = v .|. 0x11
                  expectFlags allFlags (if expected == 0 then CPU.flagZ else 0)

  describe "XOR A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for XOR A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  xorr r
                  expectExtraCycles 0
                  let expected = v `xor` if r == RegA then v else 0x11
                  RegA `registerShouldBe` expected
                  expectFlags allFlags (if expected == 0 then CPU.flagZ else 0)

  describe "XOR A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for XOR A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                xorn v
                expectExtraCycles 0
                let expected = v `xor` 0x11
                RegA `registerShouldBe` expected
                expectFlags allFlags (if expected == 0 then CPU.flagZ else 0)

  describe "XOR A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for XOR A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  xorhl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  let expected = v `xor` 0x11
                  expectFlags allFlags (if expected == 0 then CPU.flagZ else 0)

  describe "CP A, r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $
      \(r, v) ->
        it ("Works for CP A, " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInRegisters [(RegA, 0x11), (r, v)] $
              alteringFlags allFlags $
                do
                  cpr r
                  expectExtraCycles 0
                  let a = if r == RegA then v else 0x11
                  verifyArithmetic8Flags a v (-) True

  describe "CP A, n" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for CP A, " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            alteringFlags allFlags $
              do
                cpn v
                expectExtraCycles 0
                verifyArithmetic8Flags 0x11 v (-) True

  describe "CP A, (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for CP A, (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, 0x11)] $
            withValueAt RegHL 0xC000 v $
              alteringFlags allFlags $
                do
                  cphl
                  expectExtraCycles 1
                  RegHL `register16ShouldBe` 0xC000
                  0xC000 `atAddressShouldBe` v
                  verifyArithmetic8Flags 0x11 v (-) True

  describe "INC r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for INC " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags (CPU.flagH .|. CPU.flagN .|. CPU.flagZ) $
              do
                incr r
                expectExtraCycles 0
                let expected = v + 1
                r `registerShouldBe` expected
                expectFlags
                  (CPU.flagH .|. CPU.flagN .|. CPU.flagZ)
                  (buildFlags False (carryIntoBit 4 v (1 :: Word8) (+)) False (expected == 0))

  describe "INC (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for INC (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags (CPU.flagH .|. CPU.flagN .|. CPU.flagZ) $
              do
                inchl
                expectExtraCycles 2
                let expected = v + 1
                RegHL `register16ShouldBe` 0xC000
                0xC000 `atAddressShouldBe` expected
                expectFlags
                  (CPU.flagH .|. CPU.flagN .|. CPU.flagZ)
                  (buildFlags False (carryIntoBit 4 v (1 :: Word8) (+)) False (expected == 0))

  describe "DEC r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for DEC " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags (CPU.flagH .|. CPU.flagN .|. CPU.flagZ) $
              do
                decr r
                expectExtraCycles 0
                let expected = v - 1
                r `registerShouldBe` expected
                expectFlags
                  (CPU.flagH .|. CPU.flagN .|. CPU.flagZ)
                  (buildFlags False (carryIntoBit 4 v (1 :: Word8) (-)) True (expected == 0))

  describe "DEC (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for DEC (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags (CPU.flagH .|. CPU.flagN .|. CPU.flagZ) $
              do
                dechl
                expectExtraCycles 2
                let expected = v - 1
                RegHL `register16ShouldBe` 0xC000
                0xC000 `atAddressShouldBe` expected
                expectFlags
                  (CPU.flagH .|. CPU.flagN .|. CPU.flagZ)
                  (buildFlags False (carryIntoBit 4 v (1 :: Word8) (-)) True (expected == 0))

verifyArithmetic8 :: Word8 -> Word8 -> (Word32 -> Word32 -> Word32) -> Bool -> CPU.M CPUTestState ()
verifyArithmetic8 a v op n = do
  let expected = fromIntegral (fromIntegral a `op` fromIntegral v)
  RegA `registerShouldBe` expected
  verifyArithmetic8Flags a v op n

verifyArithmetic8Flags ::
  Word8 -> Word8 -> (Word32 -> Word32 -> Word32) -> Bool -> CPU.M CPUTestState ()
verifyArithmetic8Flags a v op n = do
  let expected = fromIntegral (fromIntegral a `op` fromIntegral v) :: Word8
  expectFlags
    allFlags
    (buildFlags (carryIntoBit 8 a v op) (carryIntoBit 4 a v op) n (expected == 0))

arithmetic16 :: Spec
arithmetic16 = do
  describe "ADD HL, ss" $
    for_ [(ss, (v .<<. 8) + 1) | ss <- allRegisters16, v <- [0 .. 255]] $
      \(ss, v) ->
        it ("Works for ADD HL, " <> show ss <> " ; (" <> show ss <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInSSRegisters [(RegHL, 0x11FF), (ss, v)] $
              alteringFlags (CPU.flagCY .|. CPU.flagH .|. CPU.flagN) $
                do
                  addhlss ss
                  expectExtraCycles 1
                  let a = if ss == RegHL then v else 0x11FF
                  let expected = v + a
                  RegHL `register16ShouldBe` expected
                  expectFlags
                    (CPU.flagCY .|. CPU.flagH .|. CPU.flagN)
                    (buildFlags (carryIntoBit 16 a v (+)) (carryIntoBit 12 a v (+)) False False)

  describe "ADD SP, e" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for ADD SP, " <> formatHex v) $
        withNewCPU $
          withValuesInSSRegisters [(RegSP, 0x1111)] $
            alteringFlags allFlags $
              do
                addSP v
                expectExtraCycles 2
                let a = 0x1111
                let expected = a + fromIntegral v
                RegSP `register16ShouldBe` expected
                expectFlags
                  allFlags
                  (buildFlags (carryIntoBit 8 a v (+)) (carryIntoBit 4 a v (+)) False False)

  describe "INC ss" $
    for_ [(ss, (v .<<. 8) .|. 0xFF) | ss <- allRegisters16, v <- [0 .. 255]] $
      \(ss, v) ->
        it ("Works for INC " <> show ss <> " ; (" <> show ss <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInSSRegisters [(ss, v)] $
              do
                incss ss
                expectExtraCycles 1
                ss `register16ShouldBe` (v + 1)

  describe "DEC ss" $
    for_ [(ss, (v .<<. 8) .|. 0xFF) | ss <- allRegisters16, v <- [0 .. 255]] $
      \(ss, v) ->
        it ("Works for DEC " <> show ss <> " ; (" <> show ss <> " = " <> formatHex v <> ")") $
          withNewCPU $
            withValuesInSSRegisters [(ss, v)] $
              do
                decss ss
                expectExtraCycles 1
                ss `register16ShouldBe` (v - 1)

rotateAndShift :: Spec
rotateAndShift = do
  describe "RLCA" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RLCA ; (A = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, v)] $
            alteringFlags allFlags $
              do
                rlca
                expectExtraCycles 0
                let expected = (v .<<. 1) .|. (v .>>. 7)
                RegA `registerShouldBe` expected
                expectFlags allFlags (if v `testBit` 7 then CPU.flagCY else 0)

  describe "RLA" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RLA ; (A = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, v)] $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                rla
                expectExtraCycles 0
                let expected = (v .<<. 1) .|. (if cy then 1 else 0)
                RegA `registerShouldBe` expected
                expectFlags allFlags (if v `testBit` 7 then CPU.flagCY else 0)

  describe "RRCA" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RRCA ; (A = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, v)] $
            alteringFlags allFlags $
              do
                rrca
                expectExtraCycles 0
                let expected = (v .>>. 1) .|. (v .<<. 7)
                RegA `registerShouldBe` expected
                expectFlags allFlags (if v `testBit` 0 then CPU.flagCY else 0)

  describe "RRA" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RRA ; (A = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(RegA, v)] $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                rra
                expectExtraCycles 0
                let expected = (v .>>. 1) .|. (if cy then 0x80 else 0)
                RegA `registerShouldBe` expected
                expectFlags allFlags (if v `testBit` 0 then CPU.flagCY else 0)

  describe "RLC r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for RLC " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                rlcr r
                expectExtraCycles 0
                let expected = (v .<<. 1) .|. (v .>>. 7)
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RL r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for RL " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                rlr r
                expectExtraCycles 0
                let expected = (v .<<. 1) .|. (if cy then 1 else 0)
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RRC r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for RRC " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                rrcr r
                expectExtraCycles 0
                let expected = (v .>>. 1) .|. (v .<<. 7)
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "RR r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for RR " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                rrr r
                expectExtraCycles 0
                let expected = (v .>>. 1) .|. (if cy then 0x80 else 0)
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "RLC (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RLC (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                rlchl
                expectExtraCycles 2
                let expected = (v .<<. 1) .|. (v .>>. 7)
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RL (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RL (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                rlhl
                expectExtraCycles 2
                let expected = (v .<<. 1) .|. (if cy then 1 else 0)
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "RRC (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RRC ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                rrchl
                expectExtraCycles 2
                let expected = (v .>>. 1) .|. (v .<<. 7)
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "RR (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for RR (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                rrhl
                expectExtraCycles 2
                let expected = (v .>>. 1) .|. (if cy then 0x80 else 0)
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SLA r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for SLA " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                slar r
                expectExtraCycles 0
                let expected = v .<<. 1
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "SLA (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SLA (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                slahl
                expectExtraCycles 2
                let expected = v .<<. 1
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 7) False False (expected == 0))

  describe "SRA r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for SRA " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                srar r
                expectExtraCycles 0
                let expected = (v .>>. 1) .|. (v .&. 0x80)
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SRA (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SRA (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                srahl
                expectExtraCycles 2
                let expected = (v .>>. 1) .|. (v .&. 0x80)
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SRL r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for SRL " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                srlr r
                expectExtraCycles 0
                let expected = v .>>. 1
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SRL (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SRL (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                srlhl
                expectExtraCycles 2
                let expected = v .>>. 1
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags (v `testBit` 0) False False (expected == 0))

  describe "SWAP r" $
    for_ [(r, v) | r <- allRegisters, v <- [minBound .. maxBound]] $ \(r, v) ->
      it ("Works for SWAP " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")") $
        withNewCPU $
          withValuesInRegisters [(r, v)] $
            alteringFlags allFlags $
              do
                swapr r
                expectExtraCycles 0
                let expected = (v .>>. 4) .|. (v .<<. 4)
                r `registerShouldBe` expected
                expectFlags allFlags (buildFlags False False False (expected == 0))

  describe "SWAP (HL)" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for SWAP (HL) ; ((HL) = " <> formatHex v <> ")") $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags allFlags $
              do
                swaphl
                expectExtraCycles 2
                let expected = (v .>>. 4) .|. (v .<<. 4)
                0xC000 `atAddressShouldBe` expected
                RegHL `register16ShouldBe` 0xC000
                expectFlags allFlags (buildFlags False False False (expected == 0))

bitOperations :: Spec
bitOperations = do
  describe "BIT b, r" $
    for_ [(r, b, 1 .<<. v) | r <- allRegisters, b <- [0 .. 7], v <- [0 .. 7]] $
      \(r, b, v) ->
        it ("Works for " <> formatBitR r b v) $
          withNewCPU $
            withValuesInRegisters [(r, v)] $
              alteringFlags (CPU.flagH .|. CPU.flagN .|. CPU.flagZ) $
                do
                  bitr r (fromIntegral b)
                  expectExtraCycles 0
                  r `registerShouldBe` v
                  expectFlags
                    (CPU.flagH .|. CPU.flagN .|. CPU.flagZ)
                    (buildFlags False True False (not (v `testBit` b)))

  describe "BIT b, (HL)" $
    for_ [(b, 1 .<<. v) | b <- [0 .. 7], v <- [0 .. 7]] $ \(b, v) ->
      it ("Works for " <> formatBitHL b v) $
        withNewCPU $
          withValueAt RegHL 0xC000 v $
            alteringFlags (CPU.flagH .|. CPU.flagN .|. CPU.flagZ) $
              do
                bithl (fromIntegral b)
                expectExtraCycles 1
                0xC000 `atAddressShouldBe` v
                RegHL `register16ShouldBe` 0xC000
                expectFlags
                  (CPU.flagH .|. CPU.flagN .|. CPU.flagZ)
                  (buildFlags False True False (not (v `testBit` b)))

  describe "SET b, r" $
    for_ [(r, b, 1 .<<. v) | r <- allRegisters, b <- [0 .. 7], v <- [0 .. 7]] $
      \(r, b, v) ->
        it ("Works for " <> formatSetR r b v) $
          withNewCPU $
            withValuesInRegisters [(r, v)] $ do
              setr r (fromIntegral b)
              expectExtraCycles 0
              r `registerShouldBe` (v `setBit` b)

  describe "SET b, (HL)" $
    for_ [(b, 1 .<<. v) | b <- [0 .. 7], v <- [0 .. 7]] $ \(b, v) ->
      it ("Works for " <> formatSetHL b v) $
        withNewCPU $
          withValueAt RegHL 0xC000 v $ do
            sethl (fromIntegral b)
            expectExtraCycles 2
            0xC000 `atAddressShouldBe` (v `setBit` b)
            RegHL `register16ShouldBe` 0xC000

  describe "RES b, r" $
    for_ [(r, b, 1 .<<. v) | r <- allRegisters, b <- [0 .. 7], v <- [0 .. 7]] $
      \(r, b, v) ->
        it ("Works for " <> formatResR r b v) $
          withNewCPU $
            withValuesInRegisters [(r, v)] $ do
              resr r (fromIntegral b)
              expectExtraCycles 0
              r `registerShouldBe` (v `clearBit` b)

  describe "RES b, (HL)" $
    for_ [(b, 1 .<<. v) | b <- [0 .. 7], v <- [0 .. 7]] $ \(b, v) ->
      it ("Works for " <> formatResHL b v) $
        withNewCPU $
          withValueAt RegHL 0xC000 v $ do
            reshl (fromIntegral b)
            expectExtraCycles 2
            0xC000 `atAddressShouldBe` (v `clearBit` b)
            RegHL `register16ShouldBe` 0xC000
  where
    formatBitR r b v =
      "BIT " <> show b <> ", " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")"
    formatBitHL b v = "BIT " <> show b <> ", (HL) ; ((HL) = " <> formatHex v <> ")"
    formatSetR r b v =
      "SET " <> show b <> ", " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")"
    formatSetHL b v = "SET " <> show b <> ", (HL) ; ((HL) = " <> formatHex v <> ")"
    formatResR r b v =
      "RES " <> show b <> ", " <> show r <> " ; (" <> show r <> " = " <> formatHex v <> ")"
    formatResHL b v = "RES " <> show b <> ", (HL) ; ((HL) = " <> formatHex v <> ")"

jumps :: Spec
jumps = do
  describe "JP nn" $
    it "Works for JP 4232" $
      withNewCPU $
        alteringPC $ do
          jpnn 0x4232
          expectExtraCycles 1
          expectPC 0x4232

  describe "JP (HL)" $
    it "Works for JP (HL) ; (HL = 4232)" $
      withNewCPU $
        withValuesInSSRegisters [(RegHL, 0x4232)] $
          alteringPC $
            do
              jphl
              expectExtraCycles 0
              expectPC 0x4232
              RegHL `register16ShouldBe` 0x4232

  describe "JP cc, nn" $
    for_ [(cc, shouldJump) | cc <- allConditions, shouldJump <- [True, False]] $
      \(cc, shouldJump) ->
        it ("Works for JP " <> show cc <> ", 4232 ; when condition is " <> show shouldJump) $
          withNewCPU $
            alteringFlags allFlags $
              if shouldJump
                then alteringPC $ do
                  setCondition cc
                  notAlteringFlags allFlags $ do
                    jpccnn cc 0x4232
                    expectExtraCycles 1
                    expectPC 0x4232
                else do
                  clearCondition cc
                  notAlteringFlags allFlags $ jpccnn cc 0x4232
                  expectExtraCycles 0

  describe "JR e" $
    for_ [-8 .. 8] $ \e ->
      it ("Works for JR " <> formatHex e) $
        withNewCPU $
          withPC 0x4000 $ do
            jr e
            expectExtraCycles 1
            expectPC (0x4000 + fromIntegral e)

  describe "JR cc, e" $
    for_
      [(cc, shouldJump, e) | cc <- allConditions, shouldJump <- [True, False], e <- [-8 .. 8]]
      $ \(cc, shouldJump, e) ->
        it ("Works for " <> formatJRcc cc e shouldJump) $
          withNewCPU $
            alteringFlags allFlags $
              if shouldJump
                then withPC 0x4000 $ do
                  setCondition cc
                  notAlteringFlags allFlags $ do
                    jrcc cc e
                    expectExtraCycles 1
                    expectPC (0x4000 + fromIntegral e)
                else do
                  clearCondition cc
                  notAlteringFlags allFlags $ jrcc cc e
                  expectExtraCycles 0
  where
    formatJRcc cc e shouldJump =
      "JR " <> show cc <> ", " <> formatHex e <> "; when condition is " <> show shouldJump

callAndReturn :: Spec
callAndReturn = do
  describe "CALL nn" $
    it "Works for CALL 4232" $
      withNewCPU $
        withValuesInSSRegisters [(RegSP, 0xFFF0)] $
          withPC 0x4001 $
            do
              call 0x4232
              expectExtraCycles 3
              expectPC 0x4232
              RegSP `register16ShouldBe` 0xFFEE
              0xFFEE `atAddressShouldBe` 0x01
              0xFFEF `atAddressShouldBe` 0x40

  describe "CALL cc, nn" $
    for_ [(cc, shouldCall) | cc <- allConditions, shouldCall <- [True, False]] $
      \(cc, shouldCall) ->
        it ("Works for CALL " <> show cc <> ", 4232 ; with condition " <> show shouldCall) $
          withNewCPU $
            withValuesInSSRegisters [(RegSP, 0xFFF0)] $
              alteringFlags allFlags $
                if shouldCall
                  then withPC 0x4001 $ do
                    setCondition cc
                    notAlteringFlags allFlags $ do
                      callcc cc 0x4232
                      expectExtraCycles 3
                      expectPC 0x4232
                      RegSP `register16ShouldBe` 0xFFEE
                      0xFFEE `atAddressShouldBe` 0x01
                      0xFFEF `atAddressShouldBe` 0x40
                  else do
                    clearCondition cc
                    notAlteringFlags allFlags $ do
                      callcc cc 0x4232
                      expectExtraCycles 0
                      RegSP `register16ShouldBe` 0xFFF0

  describe "RET" $
    it "Works for RET ; (SP) = 4232" $
      withNewCPU $
        withValue16At RegSP 0xFFEE 0x4232 $
          withPC 0x4001 $
            do
              ret
              expectExtraCycles 3
              expectPC 0x4232
              RegSP `register16ShouldBe` 0xFFF0

  describe "RETI" $
    for_ [True, False] $ \ime ->
      it ("Works for RETI ; (SP) = 4232, IME = " <> show ime) $
        withNewCPU $
          withValue16At RegSP 0xFFEE 0x4232 $
            withPC 0x4001 $
              withIME ime $
                do
                  reti
                  expectExtraCycles 3
                  expectPC 0x4232
                  expectIME True
                  RegSP `register16ShouldBe` 0xFFF0

  describe "RET cc" $
    for_ [(cc, shouldRet) | cc <- allConditions, shouldRet <- [True, False]] $
      \(cc, shouldRet) ->
        it ("Works for RET " <> show cc <> "; (SP) = 4232, condition is " <> show shouldRet) $
          withNewCPU $
            withValue16At RegSP 0xFFEE 0x4232 $
              alteringFlags allFlags $
                if shouldRet
                  then withPC 0x4001 $ do
                    setCondition cc
                    notAlteringFlags allFlags $ do
                      retcc cc
                      expectExtraCycles 4
                      expectPC 0x4232
                      RegSP `register16ShouldBe` 0xFFF0
                  else do
                    clearCondition cc
                    notAlteringFlags allFlags $ do
                      retcc cc
                      expectExtraCycles 1
                      RegSP `register16ShouldBe` 0xFFEE

  describe "RST t" $
    for_ [0 .. 7] $ \t ->
      it ("Works for RST " <> show t) $
        withNewCPU $
          withValuesInSSRegisters [(RegSP, 0xFFF0)] $
            withPC 0x4001 $
              do
                rst t
                expectExtraCycles 3
                expectPC (fromIntegral t * 8)
                RegSP `register16ShouldBe` 0xFFEE
                0xFFEE `atAddressShouldBe` 0x01
                0xFFEF `atAddressShouldBe` 0x40

miscellaneous :: Spec
miscellaneous = do
  describe "CPL" $
    for_ [minBound .. maxBound] $ \v ->
      it ("Works for CPL ; A = " <> formatHex v) $
        withNewCPU $
          withValuesInRegisters [(RegA, v)] $
            alteringFlags (CPU.flagH .|. CPU.flagN) $
              do
                cpl
                expectExtraCycles 0
                RegA `registerShouldBe` complement v
                expectFlags (CPU.flagH .|. CPU.flagN) (CPU.flagH .|. CPU.flagN)

  describe "NOP" $
    it "Does nothing" $
      withNewCPU $ do
        nop
        expectExtraCycles 0

  describe "CCF" $
    for_ [True, False] $ \cf0 ->
      it ("Works for CCF ; CY = " <> show cf0) $
        withNewCPU $
          alteringFlags (CPU.flagCY .|. CPU.flagH .|. CPU.flagN) $
            do
              CPU.M $ CPU.setFlagsMask CPU.flagCY (if cf0 then CPU.flagCY else 0)
              ccf
              expectExtraCycles 0
              expectFlags
                (CPU.flagCY .|. CPU.flagH .|. CPU.flagN)
                (buildFlags (not cf0) False False False)

  describe "SCF" $
    it "Works for SCF" $
      withNewCPU $
        alteringFlags (CPU.flagCY .|. CPU.flagH .|. CPU.flagN) $
          do
            scf
            expectExtraCycles 0
            expectFlags (CPU.flagCY .|. CPU.flagH .|. CPU.flagN) (buildFlags True False False False)

  describe "DI" $
    for_ [True, False] $ \ime0 ->
      it ("Works for DI ; IME = " <> show ime0) $
        withNewCPU $
          withIME ime0 $ do
            di
            expectExtraCycles 0
            expectIME False

  describe "EI" $
    for_ [True, False] $ \ime0 ->
      it ("Works for EI ; IME = " <> show ime0) $
        withNewCPU $
          withPC 0x4001 $
            withIME ime0 $ do
              ei
              expectExtraCycles 0
              expectIME ime0
              CPU.M CPU.step
              expectIME True

  describe "HALT" $ do
    for_ [True, False] $ \ime0 ->
      it ("Enters halt mode when IF = IE = 0 and IME = " <> show ime0) $
        withNewCPU $
          alteringMode $
            withIME ime0 $
              do
                halt
                expectExtraCycles 0
                expectMode CPU.ModeHalt
    it "Enters halt mode when IF & IE /= 0 and IME = True" $
      withNewCPU $
        alteringMode $
          withIME True $
            do
              CPU.M $ do
                Memory.writeByte IE 1
                Memory.writeByte IF 1
              halt
              expectExtraCycles 0
              expectMode CPU.ModeHalt
    it "Bugs out when IF & IE /= 0 and IME = False" $
      withNewCPU $
        withValuesInRegisters [(RegA, 0)] $
          withPC 0xC000 $
            withIME False $
              alteringFlags (CPU.flagZ .|. CPU.flagN .|. CPU.flagH) $
                do
                  CPU.M $ do
                    Memory.writeByte IE 1
                    Memory.writeByte IF 1
                    Memory.writeByte 0xC000 0x3C -- INC A
                  halt
                  expectExtraCycles 0
                  CPU.M CPU.step
                  CPU.M CPU.step
                  CPU.M CPU.step
                  RegA `registerShouldBe` 2
                  expectFlags (CPU.flagZ .|. CPU.flagN .|. CPU.flagH) 0

  describe "STOP" $ do
    it "Enters stop mode" $
      withNewCPU $
        alteringMode $ do
          stop
          expectExtraCycles 0
          expectMode CPU.ModeStop
    it "Switches speed mode" $
      withNewCPU $
        alteringCPUCycleClocks $ do
          KEY1 `atAddressShouldBe` 0x7E
          CPU.M $ Memory.writeByte KEY1 1
          KEY1 `atAddressShouldBe` 0x7F
          stop
          KEY1 `atAddressShouldBe` 0xFE
          expectCPUCycleClocks 2
          CPU.M $ Memory.writeByte KEY1 1
          KEY1 `atAddressShouldBe` 0xFF
          stop
          KEY1 `atAddressShouldBe` 0x7E
          expectCPUCycleClocks 4

bcd :: Spec
bcd = do
  describe "DAA after addition" $ traverse_ testAddition allBCDCombos
  describe "DAA after subtraction" $ traverse_ testSubtraction allBCDCombos
  where
    allBCDCombos = [(a, b) | a <- [0 .. 99], b <- [0 .. 99]]

    toBCD :: Word16 -> Word16
    toBCD x =
      let (r0, d0) = x `divMod` 10
          (d2, d1) = r0 `divMod` 10
       in (d2 .<<. 8) .|. (d1 .<<. 4) .|. d0
    toBCD8 = fromIntegral . toBCD . fromIntegral

    testSubtraction (a, b) =
      it ("works for " ++ show a ++ " - " ++ show b) $
        withNewCPU $
          alteringFlags allFlags $
            withValuesInRegisters [(RegA, toBCD8 a)] $
              do
                subn (toBCD8 b)
                daa
                expectExtraCycles 0
                expectFlags
                  allFlags
                  ( CPU.flagN
                      .|. (if (a - b + 100) `mod` 100 == 0 then CPU.flagZ else 0)
                      .|. (if a < b then CPU.flagCY else 0)
                  )
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                a' <- CPU.M $ CPU.readR8 RegA
                let result = fromIntegral a' + if cy then 0x0100 else 0
                let expected = if a < b then toBCD (a - b + 100) + 0x0100 else toBCD (a - b)
                liftIO (result `shouldBe` expected)

    testAddition (a, b) =
      it ("works for " ++ show a ++ " + " ++ show b) $
        withNewCPU $
          alteringFlags allFlags $
            withValuesInRegisters [(RegA, toBCD8 a)] $
              do
                addn (toBCD8 b)
                daa
                expectFlags
                  allFlags
                  ( (if (a + b) `mod` 100 == 0 then CPU.flagZ else 0)
                      .|. (if a + b >= 100 then CPU.flagCY else 0)
                  )
                cy <- CPU.M $ CPU.testFlag CPU.flagCY
                a' <- CPU.M $ CPU.readR8 RegA
                let result = fromIntegral a' + if cy then 0x0100 else 0
                liftIO (result `shouldBe` toBCD (a + b))

data InterruptBehavior = Trigger | Ignore | Wakeup deriving (Eq, Show)

isrClocks = 5

interrupts :: Spec
interrupts = do
  describe "Interrupt triggering" $
    for_
      [ (vector, pending, enabled, ime, mode)
        | vector <- [0 .. 4],
          pending <- [True, False],
          enabled <- [True, False],
          ime <- [True, False],
          mode <- [minBound .. maxBound]
      ]
      $ \(vector, pending, enabled, ime, mode) ->
        it (triggerMessage vector pending enabled ime mode) $
          withNewCPU $
            withIME ime $
              withMode mode $
                withValuesInSSRegisters [(RegSP, 0xFFF0)] $
                  withPC 0x4001 $
                    do
                      let behavior
                            | pending && enabled && ime = Trigger
                            | mode /= CPU.ModeNormal && pending && enabled = Wakeup
                            | otherwise = Ignore

                      CPU.M $ Memory.writeByte IF (if pending then bit vector else 0)
                      CPU.M $ Memory.writeByte IE (if enabled then bit vector else 0)
                      CPU.M CPU.step

                      expectExtraCycles $ case behavior of
                        Trigger -> isrClocks
                        Wakeup -> 1
                        Ignore -> 1

                      case behavior of
                        Trigger -> do
                          RegSP `register16ShouldBe` 0xFFEE
                          0xFFEE `atAddressShouldBe` 0x01
                          0xFFEF `atAddressShouldBe` 0x40
                          IF `atAddressShouldBe` 0
                          IE `atAddressShouldBe` (bit vector)
                          expectPC (fromIntegral vector * 8 + 0x40)
                          expectMode CPU.ModeNormal
                          expectIME False
                        Wakeup -> do
                          RegSP `register16ShouldBe` 0xFFF0
                          IF `atAddressShouldBe` (if pending then bit vector else 0)
                          IE `atAddressShouldBe` (if enabled then bit vector else 0)
                          expectPC 0x4002
                          expectMode CPU.ModeNormal
                          expectIME ime
                        Ignore -> do
                          RegSP `register16ShouldBe` 0xFFF0
                          IF `atAddressShouldBe` (if pending then bit vector else 0)
                          IE `atAddressShouldBe` (if enabled then bit vector else 0)
                          expectPC (if mode == CPU.ModeNormal then 0x4002 else 0x4001)
                          expectMode mode
                          expectIME ime

  describe "Invalid interrupts" $
    for_ [5, 6, 7] $ \vector ->
      it ("Does not trigger on invalid interrupt " <> show vector) $
        withNewCPU $
          withMode CPU.ModeNormal $
            withIME True $
              withPC 0x4001 $
                do
                  CPU.M $ do
                    Memory.writeByte IF (bit vector)
                    Memory.writeByte IE (bit vector)
                    CPU.step
                  expectExtraCycles 1
                  expectMode CPU.ModeNormal
                  expectIME True
                  expectPC 0x4002

  describe "Interrupt priority" $
    for_ [(l, r) | l <- [0 .. 4], r <- [l + 1 .. 4]] $ \(l, r) ->
      it ("Triggers " <> show l <> " when " <> show l <> " and " <> show r <> " are both pending") $
        withNewCPU $
          withValuesInSSRegisters [(RegSP, 0xFFF0)] $
            withMode CPU.ModeNormal $
              withIME True $
                withPC 0x4001 $
                  do
                    CPU.M $ do
                      Memory.writeByte IF (bit l .|. bit r)
                      Memory.writeByte IE (bit l .|. bit r)
                      CPU.step
                    expectExtraCycles isrClocks
                    RegSP `register16ShouldBe` 0xFFEE
                    0xFFEE `atAddressShouldBe` 0x01
                    0xFFEF `atAddressShouldBe` 0x40
                    IF `atAddressShouldBe` (bit r)
                    IE `atAddressShouldBe` (bit l .|. bit r)
                    expectPC (fromIntegral l * 8 + 0x40)
                    expectMode CPU.ModeNormal
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
