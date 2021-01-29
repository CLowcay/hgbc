{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module HGBC.Emulator
  ( Command (..),
    RuntimeConfig (..),
    Channel,
    send,
    configure,
    makeEmulatorState,
    run,
  )
where

import Control.Concurrent (forkIO, putMVar, takeMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Monad.Except (ExceptT (..), MonadIO (..), forever, mapExceptT, void, when)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT, mapWriterT)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Bits (Bits ((.&.)))
import qualified Data.ByteString as B
import Data.Functor.Identity (Identity)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import HGBC.Config (Config (..))
import qualified HGBC.Config.CommandLine as CommandLine
import qualified HGBC.Config.Paths as Path
import qualified HGBC.Debugger.Breakpoints as Breakpoints
import qualified HGBC.Debugger.Disassembly as Disassembly
import qualified HGBC.Debugger.Labels as Labels
import qualified HGBC.Debugger.State as DebugState
import HGBC.Errors (FileParseErrors)
import qualified HGBC.Events as Event
import Machine.GBC.CPU (readPC)
import qualified Machine.GBC.CPU as CPU
import qualified Machine.GBC.Color as Color
import Machine.GBC.Disassembler (LongAddress (..), disassembleFrom, disassemblyRequired)
import qualified Machine.GBC.Emulator as Emulator
import qualified Machine.GBC.Graphics as Graphics
import Machine.GBC.Memory (getBank)
import Machine.GBC.Mode (EmulatorMode (DMG))
import qualified Machine.GBC.ROM as ROM
import qualified Machine.GBC.Serial as Serial
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import UnliftIO.Exception
  ( Exception (displayException),
    IOException,
    catch,
    try,
  )

-- | A notification for the emulator thread.
data Command
  = Pause
  | Quit
  | Run
  | RunTo LongAddress
  | Step
  | StepOver
  | StepOut
  | Restart
  deriving (Eq, Ord, Show)

-- | A communication channel for the emulator thread.
newtype Channel = Channel (TChan Command)

-- | Send a 'Command' to an 'Emulator'.
send :: MonadIO m => Channel -> Command -> m ()
send (Channel commandChannel) = liftIO . atomically . writeTChan commandChannel

-- | Receieve a 'Command' if there is one waiting.
getCommand :: MonadIO m => Channel -> m (Maybe Command)
getCommand (Channel commandChannel) = liftIO $ atomically $ tryReadTChan commandChannel

-- | Block until a 'Command' arrives.
waitCommand :: MonadIO m => Channel -> m Command
waitCommand (Channel commandChannel) = liftIO $ atomically $ readTChan commandChannel

-- | Configuration required to run the emulator.
data RuntimeConfig = RuntimeConfig
  { debugMode :: Bool,
    showStats :: Bool,
    romFileName :: FilePath,
    debugState :: DebugState.DebugState,
    commandChannel :: Channel,
    eventChannel :: Event.Channel
  }

-- | Create the 'RuntimeConfig'.
configure :: CommandLine.Options -> Config k Identity -> IO RuntimeConfig
configure options config = do
  let debugMode = CommandLine.debugMode options
  let showStats = CommandLine.stats options
  let romFileName = CommandLine.filename options
  commandChannel <- Channel <$> newTChanIO
  eventChannel <- Event.newChannel
  debugState <- DebugState.init (CommandLine.filename options) (bootROM config)
  pure RuntimeConfig {..}

-- | Create a 'GBC.EmulatorState'.
makeEmulatorState ::
  FilePath ->
  Config k Identity ->
  Graphics.Sync ->
  Ptr Word8 ->
  ExceptT FileParseErrors (WriterT [FileParseErrors] IO) Emulator.State
makeEmulatorState filename Config {..} graphicsSync frameBuffer = do
  romPaths <- liftIO (Path.romPaths filename)
  bootROMData <- traverse tryReadFile bootROM
  fileContent <- tryReadFile filename
  rom <- attachFilename filename (ROM.parse romPaths fileContent)
  liftIO $ do
    when (ROM.requiresSaveFiles rom) $
      createDirectoryIfMissing True (takeDirectory (ROM.romSaveFile (ROM.romPaths rom)))

    serialSync <- nullSerial
    s <-
      Emulator.init
        bootROMData
        rom
        mode
        (Color.correction colorCorrection)
        serialSync
        graphicsSync
        frameBuffer

    when (Emulator.mode s == DMG) $
      liftIO $ do
        Emulator.writeBgPalette s 0 backgroundPalette
        Emulator.writeSpritePalette s 0 sprite1Palette
        Emulator.writeSpritePalette s 1 sprite2Palette

    pure s
  where
    attachFilename path =
      mapExceptT
        ( mapWriterT
            (fmap (bimap (first (\err -> (path, [err]))) (\errs -> [(path, errs) | not (null errs)])))
        )
    tryReadFile path = ExceptT (first (convertIOException path) <$> liftIO (try (B.readFile path)))
    convertIOException path err = (path, [displayException (err :: IOException)])

    nullSerial = do
      sync <- Serial.newSync
      void $
        forkIO $
          forever $ do
            void (takeMVar (Serial.out sync))
            putMVar (Serial.inp sync) 0xFF
      pure sync

-- | Run the emulator.  Does not return until the Quit command is sent.
run :: RuntimeConfig -> ReaderT Emulator.State IO ()
run RuntimeConfig {..} = do
  CPU.reset
  isFault <-
    (if debugMode then pauseLoop else runEmulatorLoop Nothing Nothing)
      `catch` ( \fault -> do
                  Event.send eventChannel (Event.Fault fault)
                  pure True
              )
  when isFault faultLoop
  where
    faultLoop = do
      isFault <- pauseLoop2
      if isFault then faultLoop else pure ()

    runEmulatorLoop runToAddress level = do
      Event.send eventChannel Event.Resumed
      emulatorLoop runToAddress level

    emulatorLoop runToAddress level = do
      emulatorClock <- Emulator.getClock
      now <- liftIO getSystemTime
      let innerEmulatorLoop !steps = do
            Emulator.step
            breakRequired <-
              if debugMode
                then do
                  address <- getCurrentAddress
                  updateDisassembly address
                  breakpoint <- liftIO $ Breakpoints.check debugState address
                  callDepth <- CPU.getCallDepth
                  pure (Just address == runToAddress || breakpoint || Just callDepth == level)
                else pure False

            if
                | breakRequired -> pauseLoop
                | steps .&. 0x1FFFFF == 0 -> do
                  when showStats $ statisticsUpdate emulatorClock now
                  emulatorLoop runToAddress level
                | otherwise -> do
                  command <- getCommand commandChannel
                  case command of
                    Just Pause -> pauseLoop
                    Just Quit -> pure False
                    Just Restart -> CPU.reset >> innerEmulatorLoop (steps + 1)
                    _ -> innerEmulatorLoop (steps + 1)
      innerEmulatorLoop (1 :: Int)

    pauseLoop = do
      Event.send eventChannel Event.Paused
      pauseLoop2

    pauseLoop2 = do
      command <- waitCommand commandChannel
      case command of
        Quit -> pure False
        Pause -> runEmulatorLoop Nothing Nothing
        Run -> runEmulatorLoop Nothing Nothing
        RunTo address -> runEmulatorLoop (Just address) Nothing
        Step -> singleStep >> pauseLoop
        StepOver -> do
          callDepth0 <- CPU.getCallDepth
          Event.send eventChannel Event.Resumed >> singleStep
          callDepth1 <- CPU.getCallDepth
          if callDepth1 > callDepth0 then emulatorLoop Nothing (Just callDepth0) else pauseLoop
        StepOut -> do
          callDepth <- CPU.getCallDepth
          runEmulatorLoop Nothing (Just (callDepth - 1))
        Restart -> CPU.reset >> pauseLoop

    singleStep = Emulator.step >> getCurrentAddress >>= updateDisassembly

    updateDisassembly address@(LongAddress _ pc) = do
      disassembly <- liftIO (Disassembly.get debugState)
      r <- disassemblyRequired address disassembly
      when r $ do
        (disassembly', newLabels) <- disassembleFrom pc disassembly
        liftIO $ do
          Disassembly.set debugState disassembly'
          Labels.addFromList debugState eventChannel newLabels

    getCurrentAddress = do
      pc <- readPC
      bank <- getBank pc
      pure (LongAddress bank pc)

    statisticsUpdate :: Int -> SystemTime -> ReaderT Emulator.State IO ()
    statisticsUpdate emulatorClock now = do
      emulatorClock' <- Emulator.getClock
      liftIO $ do
        now' <- getSystemTime
        Event.send
          eventChannel
          ( Event.Statistics
              (systemTimeToDouble now' - systemTimeToDouble now)
              (emulatorClock' - emulatorClock)
          )

    systemTimeToDouble (MkSystemTime s n) = fromIntegral s + (fromIntegral n / 1000000000)
