{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.List
import           Data.Word
import           Foreign.Marshal.Alloc
import           Machine.GBC
import           Machine.GBC.Util               ( formatHex )
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           Test.Hspec
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as LB
import qualified Machine.GBC.CPU               as CPU
import qualified Machine.GBC.Graphics          as Graphics
import qualified Machine.GBC.Memory            as Memory
import qualified Machine.GBC.Serial            as Serial

main :: IO ()
main = do
  blarggDir <- lookupEnv "BLARGG_DIR"
  hspec $ maybe (pure ()) blargg blarggDir

  mooneyeDir <- lookupEnv "MOONEYE_DIR"
  hspec $ maybe (pure ()) mooneye mooneyeDir

blargg :: FilePath -> SpecWith ()
blargg blarggPath = describe "blargg suite" $ do
  specify "cpu_instrs" $ do
    output <- blarggTestSerial (blarggPath </> "cpu_instrs.gb") 0x06f1
    output
      `shouldBe` "cpu_instrs\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  \n\nPassed all tests\n"
  specify "instr_timing" $ do
    output <- blarggTestSerial (blarggPath </> "instr_timing.gb") 0xC8B0
    output `shouldBe` "instr_timing\n\n\nPassed\n"
  specify "mem_timing" $ do
    output <- blarggTestInMemory (blarggPath </> "mem_timing.gb") 0x2BDD
    output `shouldBe` "mem_timing\n\n01:ok  02:ok  03:ok  \n\nPassed\n"
  specify "cgb_sound" $ do
    output <- blarggTestInMemory (blarggPath </> "cgb_sound.gb") 0x2BD4
    output
      `shouldBe` "cgb_sound\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  12:ok  \n\nPassed\n"
  specify "halt_bug" $ do
    output <- blarggTestInMemory (blarggPath </> "halt_bug.gb") 0xC818
    output
      `shouldBe` "halt bug\n\nIE IF IF DE\n01 10 F1 0C04 \n01 00 E1 0C04 \n01 01 E1 0411 \n11 00 E1 0C04 \n11 10 F1 0411 \n11 11 F1 0411 \nE1 00 E1 0C04 \nE1 E0 E1 0C04 \nE1 E1 E1 0411 \n\nPassed\n"

mooneye :: FilePath -> Spec
mooneye mooneyePath = do
  let bitsPath       = mooneyePath </> "bits"
  let instrPath      = mooneyePath </> "instr"
  let interruptsPath = mooneyePath </> "interrupts"
  let oamDMAPath     = mooneyePath </> "oam_dma"
  let ppuPath        = mooneyePath </> "ppu"
  let serialPath     = mooneyePath </> "serial"
  let timerPath      = mooneyePath </> "timer"
  tests           <- runIO (getRomsInOrder <$> listDirectory mooneyePath)
  bitsTests       <- runIO (getRomsInOrder <$> listDirectory bitsPath)
  instrTests      <- runIO (getRomsInOrder <$> listDirectory instrPath)
  interruptsTests <- runIO (getRomsInOrder <$> listDirectory interruptsPath)
  oamDMATests     <- runIO (getRomsInOrder <$> listDirectory oamDMAPath)
  ppuTests        <- runIO (getRomsInOrder <$> listDirectory ppuPath)
  serialTests     <- runIO (getRomsInOrder <$> listDirectory serialPath)
  timerTests      <- runIO (getRomsInOrder <$> listDirectory timerPath)
  describe "mooneye suite" $ do
    for_ tests (testROM mooneyePath)
    describe "bits" $ for_ bitsTests (testROM bitsPath)
    describe "instr" $ for_ instrTests (testROM instrPath)
    describe "interrupts" $ for_ interruptsTests (testROM interruptsPath)
    describe "oam_dma" $ for_ oamDMATests (testROM oamDMAPath)
    describe "ppu" $ for_ ppuTests (testROM ppuPath)
    describe "serial" $ for_ serialTests (testROM serialPath)
    describe "timer" $ for_ timerTests (testROM timerPath)

 where
  getRomsInOrder = sort . filter ((".gb" ==) . takeExtension)
  testROM path rom = specify rom $ do
    (result, output) <- mooneyeTest (path </> rom)
    when (result /= Passed) $ B.putStrLn output
    result `shouldBe` Passed

blarggTestSerial :: FilePath -> Word16 -> IO B.ByteString
blarggTestSerial filename terminalAddress = romTest filename
                                                    accumulateSerialOutput
                                                    (terminateAtAddress terminalAddress)
                                                    (liftIO . getResult)
  where getResult buffer = LB.toStrict . BB.toLazyByteString <$> readIORef buffer

blarggTestInMemory :: FilePath -> Word16 -> IO B.ByteString
blarggTestInMemory filename terminalAddress = romTest filename
                                                      ignoreSerialOutput
                                                      (terminateAtAddress terminalAddress)
                                                      getResult
 where
  getResult _ = do
    Memory.writeByte 0 0x0A
    LB.toStrict . BB.toLazyByteString <$> readString 0xA004

data MooneyeResult = Passed
                   | HardwareFailures Int Word16
                   | HardwareTestFailed
                   deriving Eq

instance Show MooneyeResult where
  show Passed                  = "Passed"
  show HardwareTestFailed      = "HardwareTestFailed"
  show (HardwareFailures n pc) = "HardwareFailures " <> show n <> " " <> formatHex pc

mooneyeTest :: FilePath -> IO (MooneyeResult, B.ByteString)
mooneyeTest filename = romTest filename accumulateSerialOutput terminateOnMagic getResult
 where
  terminateOnMagic = do
    pc              <- CPU.readPC
    nextInstruction <- Memory.readByte pc
    pure (nextInstruction == 0x40)
  getResult buffer = do
    stringResult          <- LB.toStrict . BB.toLazyByteString <$> liftIO (readIORef buffer)
    CPU.RegisterFile {..} <- CPU.getRegisterFile
    let testResult
          | regA /= 0 = HardwareFailures (fromIntegral regA) regPC
          | regB /= 3 || regC /= 5 || regD /= 8 || regE /= 13 || regH /= 21 || regL /= 34 = HardwareTestFailed
          | otherwise = Passed
    pure (testResult, stringResult)

data TestComplete = TestComplete deriving (Eq, Show)
data Timeout = Timeout deriving (Eq, Show)
instance Exception TestComplete
instance Exception Timeout

romTest
  :: FilePath
  -> SerialHandler
  -> ReaderT EmulatorState IO Bool
  -> (IORef BB.Builder -> ReaderT EmulatorState IO a)
  -> IO a
romTest filename serialHandler terminate getResult =
  withSystemTempDirectory "rom-testing" $ \tempDir -> do
    let baseName = takeBaseName filename
    createDirectoryIfMissing True (tempDir </> baseName)
    let paths = ROMPaths { romFile     = filename
                         , romSaveFile = tempDir </> baseName </> "battery"
                         , romRTCFile  = tempDir </> baseName </> "rtc"
                         }

    romData          <- B.readFile filename
    (eROM, warnings) <- runWriterT (runExceptT (parseROM paths romData))
    unless (null warnings) $ fail (show warnings)
    case eROM of
      Left  err -> fail (show err)
      Right rom -> allocaBytes (160 * 144 * 4) $ \frameBuffer -> do
        serialSync <- Serial.newSync
        gs         <- newSync
        buffer     <- newIORef ""
        bracket (serialHandler serialSync buffer) (`throwTo` TestComplete) $ \_ ->
          bracket (nullGraphics gs) (`throwTo` TestComplete) $ \_ -> do
            emulatorState <- initEmulatorState Nothing
                                               rom
                                               Nothing
                                               DefaultColorCorrection
                                               serialSync
                                               gs
                                               frameBuffer
            runReaderT (reset >> runLoop timeout >> getResult buffer) emulatorState

 where
  nullGraphics gs = forkIO $ foreverUntil TestComplete $ do
    takeMVar (Graphics.signalWindow gs)
    putMVar (Graphics.bufferAvailable gs) ()

  runLoop 0               = liftIO (throwIO Timeout)
  runLoop !remainingSteps = do
    step
    terminateNow <- terminate
    if terminateNow then pure () else runLoop (remainingSteps - 1)

timeout :: Int
timeout = 1024 * 1024 * 60 * 3

readString :: Word16 -> ReaderT EmulatorState IO BB.Builder
readString = readString0 ""
 where
  readString0 !acc addr = do
    b <- Memory.readByte addr
    if b == 0 then pure acc else readString0 (acc <> BB.word8 b) (addr + 1)

type SerialHandler = Serial.Sync -> IORef BB.Builder -> IO ThreadId

accumulateSerialOutput :: SerialHandler
accumulateSerialOutput sync buffer = forkIO $ foreverUntil TestComplete $ do
  byte <- takeMVar (Serial.out sync)
  putMVar (Serial.inp sync) 0xFF
  modifyIORef' buffer (<> BB.word8 byte)

ignoreSerialOutput :: SerialHandler
ignoreSerialOutput sync _ = forkIO $ foreverUntil TestComplete $ do
  void (takeMVar (Serial.out sync))
  putMVar (Serial.inp sync) 0xFF

foreverUntil :: (Exception e, Eq e) => e -> IO a -> IO ()
foreverUntil e action = forever action `catch` (\ex -> if e == ex then pure () else throwIO ex)

terminateAtAddress :: Word16 -> ReaderT EmulatorState IO Bool
terminateAtAddress address = CPU.readPC <&> (== address)
