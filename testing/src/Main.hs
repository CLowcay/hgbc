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
import           Data.IORef
import           Data.Word
import           Foreign.Marshal.Alloc
import           Machine.GBC
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           Test.Hspec
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as LB
import qualified Machine.GBC.CPU               as CPU
import qualified Machine.GBC.Graphics          as Graphics
import qualified Machine.GBC.Memory            as Memory
import qualified Machine.GBC.Serial            as Serial

main :: IO ()
main = do
  blarggDir <- lookupEnv "BLARGG_DIR"
  hspec $ maybe (pure ()) blargg blarggDir

blargg :: FilePath -> SpecWith ()
blargg blarggPath = describe "blargg suite" $ do
  specify "cpu_instrs" $ do
    output <- blarggTest (blarggPath </> "cpu_instrs.gb") Serial 0x06f1
    output
      `shouldBe` "cpu_instrs\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  \n\nPassed all tests\n"
  specify "instr_timing" $ do
    output <- blarggTest (blarggPath </> "instr_timing.gb") Serial 0xC8B0
    output `shouldBe` "instr_timing\n\n\nPassed\n"
  specify "mem_timing" $ do
    output <- blarggTest (blarggPath </> "mem_timing.gb") InMemory 0x2BDD
    output `shouldBe` "mem_timing\n\n01:ok  02:ok  03:ok  \n\nPassed\n"
  specify "cgb_sound" $ do
    output <- blarggTest (blarggPath </> "cgb_sound.gb") InMemory 0x2BD4
    output
      `shouldBe` "cgb_sound\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  12:ok  \n\nPassed\n"
  specify "halt_bug" $ do
    output <- blarggTest (blarggPath </> "halt_bug.gb") InMemory 0xC818
    output
      `shouldBe` "halt bug\n\nIE IF IF DE\n01 10 F1 0C04 \n01 00 E1 0C04 \n01 01 E1 0411 \n11 00 E1 0C04 \n11 10 F1 0411 \n11 11 F1 0411 \nE1 00 E1 0C04 \nE1 E0 E1 0C04 \nE1 E1 E1 0411 \n\nPassed\n"

data TestComplete = TestComplete deriving (Eq, Show)
instance Exception TestComplete

data BlarggOutputStyle = Serial | InMemory deriving (Eq, Show)

blarggTest :: FilePath -> BlarggOutputStyle -> Word16 -> IO B.ByteString
blarggTest filename outputStyle terminalAddress =
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
            runReaderT (reset >> runLoop) emulatorState
            case outputStyle of
              InMemory -> flip runReaderT emulatorState $ do
                Memory.writeByte 0 0x0A
                liftIO . writeIORef buffer =<< readString 0xA004
              _ -> pure ()
        LB.toStrict . BB.toLazyByteString <$> readIORef buffer

 where
  runLoop = do
    step
    pc <- CPU.readPC
    if pc == terminalAddress then pure () else runLoop

  readString = readString0 ""
   where
    readString0 !acc addr = do
      b <- Memory.readByte addr
      if b == 0 then pure acc else readString0 (acc <> BB.word8 b) (addr + 1)

  serialHandler sync buffer = case outputStyle of
    Serial   -> accumulateSerialOutput sync buffer
    InMemory -> ignoreSerialOutput sync

  ignoreSerialOutput sync = forkIO $ foreverUntil TestComplete $ do
    void (takeMVar (Serial.out sync))
    putMVar (Serial.inp sync) 0xFF
  accumulateSerialOutput sync buffer = forkIO $ foreverUntil TestComplete $ do
    byte <- takeMVar (Serial.out sync)
    putMVar (Serial.inp sync) 0xFF
    modifyIORef' buffer (<> BB.word8 byte)
  nullGraphics gs = forkIO $ foreverUntil TestComplete $ do
    takeMVar (Graphics.signalWindow gs)
    putMVar (Graphics.bufferAvailable gs) ()
  foreverUntil e action = forever action `catch` (\ex -> if e == ex then pure () else throwIO ex)
