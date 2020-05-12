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
import           Data.Functor
import           Data.IORef
import           Data.Word
import           Foreign.Marshal.Alloc
import           Framework
import           Machine.GBC.Util               ( formatHex )
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as LB
import           Data.List
import           Data.Traversable
import qualified Machine.GBC.CPU               as CPU
import qualified Machine.GBC.Color             as Color
import qualified Machine.GBC.Emulator          as Emulator
import qualified Machine.GBC.Graphics          as Graphics
import qualified Machine.GBC.Memory            as Memory
import qualified Machine.GBC.ROM               as ROM
import qualified Machine.GBC.Serial            as Serial
import           Data.Time

main :: IO ()
main = do
  blarggDir <- lookupEnv "BLARGG_DIR"
  let blarggTestTree = case blarggDir of
        Nothing   -> []
        Just path -> [blarggSuite path]

  mooneyeDir      <- lookupEnv "MOONEYE_DIR"
  mooneyeTestTree <- case mooneyeDir of
    Nothing   -> pure []
    Just path -> pure <$> mooneyeSuite "mooneye" path

  testTime   <- getCurrentTime
  results    <- runTestSuite (TestTree "H-GBC ROM tests" () (blarggTestTree ++ mooneyeTestTree))
  reportTime <- getCurrentTime
  LB.writeFile "hgbc-test-report.html"
               (generateReport "H-GBC ROM test results" testTime reportTime results)
  checkResultsAndExit results

blarggSuite :: FilePath -> TestSuite
blarggSuite blarggPath = TestTree
  "Blargg Suite"
  ()
  [ TestCase "cpu_instrs" Required $ do
    output <- blarggTestSerial (blarggPath </> "cpu_instrs.gb") 0x06f1
    pure (if output == cpu_instrs_output then TestPassed else TestFailed (B.unpack output))
  , TestCase "instr_timing" Required $ do
    output <- blarggTestSerial (blarggPath </> "instr_timing.gb") 0xC8B0
    pure (if output == instr_timing_output then TestPassed else TestFailed (B.unpack output))
  , TestCase "mem_timing" Required $ do
    output <- blarggTestInMemory (blarggPath </> "mem_timing.gb") 0x2BDD
    pure (if output == mem_timing_output then TestPassed else TestFailed (B.unpack output))
  , TestCase "cgb_sound" Required $ do
    output <- blarggTestInMemory (blarggPath </> "cgb_sound.gb") 0x2BD4
    pure (if output == cgb_sound_output then TestPassed else TestFailed (B.unpack output))
  , TestCase "halt_bug" Required $ do
    output <- blarggTestInMemory (blarggPath </> "halt_bug.gb") 0xC818
    pure (if output == halt_bug_output then TestPassed else TestFailed (B.unpack output))
  ]

 where
  cpu_instrs_output
    = "cpu_instrs\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  \n\nPassed all tests\n"
  instr_timing_output = "instr_timing\n\n\nPassed\n"
  mem_timing_output   = "mem_timing\n\n01:ok  02:ok  03:ok  \n\nPassed\n"
  cgb_sound_output
    = "cgb_sound\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  12:ok  \n\nPassed\n"
  halt_bug_output
    = "halt bug\n\nIE IF IF DE\n01 10 F1 0C04 \n01 00 E1 0C04 \n01 01 E1 0411 \n11 00 E1 0C04 \n11 10 F1 0411 \n11 11 F1 0411 \nE1 00 E1 0C04 \nE1 E0 E1 0C04 \nE1 E1 E1 0411 \n\nPassed\n"

mooneyeSuite :: String -> FilePath -> IO TestSuite
mooneyeSuite name path = do
  listing <- listDirectory path
  dirs    <- filterM (doesDirectoryExist . (path </>)) listing
  let tests = getRomsInOrder listing <&> \rom -> TestCase
        rom
        (if isOptionalMooneyeTest (path </> rom) then Optional else Required)
        (testROM path rom)
  subtests <- for (filter (`notElem` skipMooneyeTests) dirs)
    $ \dir -> mooneyeSuite dir (path </> dir)
  pure (TestTree name () (tests ++ subtests))
 where
  getRomsInOrder = sort . filter ((".gb" ==) . takeExtension)
  testROM fullPath rom = do
    result <- mooneyeTest (fullPath </> rom)
    pure $ case result of
      Passed  -> TestPassed
      failure -> TestFailed (show failure)

isOptionalMooneyeTest :: FilePath -> Bool
isOptionalMooneyeTest rom =
  let romName = takeBaseName rom
  in  ("-GS" `isSuffixOf` romName)
        || ("ppu" `isInfixOf` rom)
        || ("mbc2" `isInfixOf` rom)
        || ("hwio" `isInfixOf` rom)
        || ("boot_" `isPrefixOf` romName)
        || ("multicart_rom" `isInfixOf` romName)

skipMooneyeTests :: [String]
skipMooneyeTests = ["utils", "manual-only", "madness"]

blarggTestSerial :: FilePath -> Word16 -> IO B.ByteString
blarggTestSerial filename terminalAddress = romTest filename
                                                    accumulateSerialOutput
                                                    (terminateAtAddress terminalAddress)
                                                    (liftIO . getResult)
                                                    (fmap B.unpack . liftIO . getResult)
  where getResult buffer = LB.toStrict . BB.toLazyByteString <$> readIORef buffer

blarggTestInMemory :: FilePath -> Word16 -> IO B.ByteString
blarggTestInMemory filename terminalAddress = romTest filename
                                                      ignoreSerialOutput
                                                      (terminateAtAddress terminalAddress)
                                                      getResult
                                                      (fmap B.unpack . getResult)
 where
  getResult _ = do
    Memory.writeByte 0 0x0A
    LB.toStrict . BB.toLazyByteString <$> readString 256 0xA004

data MooneyeResult = Passed
                   | HardwareFailures Int Word16
                   | HardwareTestFailed
                   deriving Eq

instance Show MooneyeResult where
  show Passed                  = "Passed"
  show HardwareTestFailed      = "HardwareTestFailed"
  show (HardwareFailures n pc) = "HardwareFailures " <> show n <> " " <> formatHex pc

mooneyeTest :: FilePath -> IO MooneyeResult
mooneyeTest filename = romTest filename
                               accumulateSerialOutput
                               terminateOnMagic
                               getResult
                               (const (pure "Failed"))
 where
  terminateOnMagic = do
    pc              <- CPU.readPC
    nextInstruction <- Memory.readByte pc
    pure (nextInstruction == 0x40)
  getResult _ = do
    CPU.RegisterFile {..} <- CPU.getRegisterFile
    let testResult
          | regA /= 0 = HardwareFailures (fromIntegral regA) regPC
          | regB /= 3 || regC /= 5 || regD /= 8 || regE /= 13 || regH /= 21 || regL /= 34 = HardwareTestFailed
          | otherwise = Passed
    pure testResult

data TestComplete = TestComplete deriving (Eq, Show)
newtype Timeout = Timeout String deriving Eq
instance Show Timeout where
  show (Timeout result) = "Timeout\n" ++ result
instance Exception TestComplete
instance Exception Timeout

romTest
  :: FilePath
  -> SerialHandler
  -> ReaderT Emulator.State IO Bool
  -> (IORef BB.Builder -> ReaderT Emulator.State IO a)
  -> (IORef BB.Builder -> ReaderT Emulator.State IO String)
  -> IO a
romTest filename serialHandler terminate getResult timeoutHandler =
  withSystemTempDirectory "rom-testing" $ \tempDir -> do
    let baseName = takeBaseName filename
    createDirectoryIfMissing True (tempDir </> baseName)
    let paths = ROM.Paths { ROM.romFile     = filename
                          , ROM.romSaveFile = tempDir </> baseName </> "battery"
                          , ROM.romRTCFile  = tempDir </> baseName </> "rtc"
                          }

    romData          <- B.readFile filename
    (eROM, warnings) <- runWriterT (runExceptT (ROM.parse paths romData))
    unless (null warnings) $ fail (show warnings)
    case eROM of
      Left  err -> fail (show err)
      Right rom -> allocaBytes (160 * 144 * 4) $ \frameBuffer -> do
        serialSync <- Serial.newSync
        gs         <- Graphics.newSync
        buffer     <- newIORef ""
        bracket (serialHandler serialSync buffer) (`throwTo` TestComplete) $ \_ ->
          bracket (nullGraphics gs) (`throwTo` TestComplete) $ \_ -> do
            emulatorState <- Emulator.init Nothing
                                           rom
                                           Nothing
                                           (Color.correction Color.NoCorrection)
                                           serialSync
                                           gs
                                           frameBuffer
            runReaderT (CPU.reset >> runLoop buffer timeout >> getResult buffer) emulatorState

 where
  nullGraphics gs = forkIO $ foreverUntil TestComplete $ do
    takeMVar (Graphics.signalWindow gs)
    putMVar (Graphics.bufferAvailable gs) ()

  runLoop buffer 0               = liftIO . throwIO =<< Timeout <$> timeoutHandler buffer
  runLoop buffer !remainingSteps = do
    Emulator.step
    terminateNow <- terminate
    if terminateNow then pure () else runLoop buffer (remainingSteps - 1)

timeout :: Int
timeout = 1024 * 1024 * 30

readString :: Int -> Word16 -> ReaderT Emulator.State IO BB.Builder
readString limit0 = readString0 limit0 ""
 where
  readString0 0      acc  _    = pure acc
  readString0 !limit !acc addr = do
    b <- Memory.readByte addr
    if b == 0 then pure acc else readString0 (limit - 1) (acc <> BB.word8 b) (addr + 1)

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

terminateAtAddress :: Word16 -> ReaderT Emulator.State IO Bool
terminateAtAddress address = CPU.readPC <&> (== address)
