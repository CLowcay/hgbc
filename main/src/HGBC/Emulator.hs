{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module HGBC.Emulator
  ( Command(..)
  , RuntimeConfig(..)
  , Channel
  , send
  , configure
  , makeEmulatorState
  , run
  )
where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Bifunctor
import           Data.Bits
import           Data.Functor.Identity
import           Data.Time.Clock.System
import           Data.Word
import           Foreign.Ptr
import           HGBC.Config                    ( Config(..) )
import           HGBC.Errors
import           Machine.GBC.CPU                ( readPC )
import           Machine.GBC.Disassembler
import           Machine.GBC.Memory             ( getBank )
import           System.Directory
import           System.FilePath
import qualified Data.ByteString               as B
import qualified HGBC.Config.CommandLine       as CommandLine
import qualified HGBC.Config.Paths             as Path
import qualified HGBC.Debugger.Breakpoints     as Breakpoints
import qualified HGBC.Debugger.Disassembly     as Disassembly
import qualified HGBC.Debugger.Labels          as Labels
import qualified HGBC.Debugger.State           as DebugState
import qualified HGBC.Events                   as Event
import qualified Machine.GBC                   as GBC

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
data RuntimeConfig = RuntimeConfig {
    debugMode :: Bool
  , showStats :: Bool
  , romFileName :: FilePath
  , debugState :: DebugState.DebugState
  , commandChannel :: Channel
  , eventChannel :: Event.Channel
}

-- | Create the 'RuntimeConfig'.
configure :: CommandLine.Options -> Config k Identity -> IO RuntimeConfig
configure options config = do
  let debugMode   = CommandLine.debugMode options
  let showStats   = CommandLine.stats options
  let romFileName = CommandLine.filename options
  commandChannel <- Channel <$> newTChanIO
  eventChannel   <- Event.newChannel
  debugState     <- DebugState.init (CommandLine.filename options) (bootROM config)
  pure RuntimeConfig { .. }

-- | Create a 'GBC.EmulatorState'.
makeEmulatorState
  :: FilePath
  -> Config k Identity
  -> GBC.Sync
  -> Ptr Word8
  -> ExceptT FileParseErrors (WriterT [FileParseErrors] IO) GBC.EmulatorState
makeEmulatorState filename Config {..} graphicsSync frameBuffer = do
  romPaths    <- liftIO (Path.romPaths filename)
  bootROMData <- traverse tryReadFile bootROM
  fileContent <- tryReadFile filename
  rom         <- attachFilename filename (GBC.parseROM romPaths fileContent)
  liftIO $ do
    when (GBC.requiresSaveFiles rom)
      $ createDirectoryIfMissing True (takeDirectory (GBC.romSaveFile (GBC.romPaths rom)))

    s <- GBC.initEmulatorState bootROMData rom mode colorCorrection graphicsSync frameBuffer

    when (GBC.mode s == GBC.DMG) $ liftIO $ do
      GBC.writeBgRGBPalette s 0 backgroundPalette
      GBC.writeFgRGBPalette s 0 sprite1Palette
      GBC.writeFgRGBPalette s 1 sprite2Palette

    pure s

 where
  attachFilename path = mapExceptT
    (mapWriterT
      (fmap (bimap (first (\err -> (path, [err]))) (\errs -> [ (path, errs) | not (null errs) ])))
    )
  tryReadFile path = ExceptT (first (convertIOException path) <$> liftIO (try (B.readFile path)))
  convertIOException path err = (path, [displayException (err :: IOException)])

-- | Run the emulator.  Does not return until the Quit command is sent.
run :: RuntimeConfig -> ReaderT GBC.EmulatorState IO ()
run RuntimeConfig {..} = do
  GBC.reset
  if debugMode then pauseLoop else runEmulatorLoop Nothing Nothing

 where
  runEmulatorLoop runToAddress level = do
    Event.send eventChannel Event.Resumed
    emulatorLoop runToAddress level

  emulatorLoop runToAddress level = do
    emulatorClock <- GBC.getEmulatorClock
    now           <- liftIO getSystemTime
    let innerEmulatorLoop !steps = do
          GBC.step
          breakRequired <- if debugMode
            then do
              address <- getCurrentAddress
              updateDisassembly address
              breakpoint <- liftIO $ Breakpoints.check debugState address
              callDepth  <- GBC.getCallDepth
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
                Just Pause   -> pauseLoop
                Just Quit    -> pure ()
                Just Restart -> GBC.reset >> innerEmulatorLoop (steps + 1)
                _            -> innerEmulatorLoop (steps + 1)
    innerEmulatorLoop (1 :: Int)

  pauseLoop = do
    Event.send eventChannel Event.Paused
    command <- waitCommand commandChannel
    case command of
      Quit          -> pure ()
      Pause         -> runEmulatorLoop Nothing Nothing
      Run           -> runEmulatorLoop Nothing Nothing
      RunTo address -> runEmulatorLoop (Just address) Nothing
      Step          -> singleStep >> pauseLoop
      StepOver      -> do
        callDepth0 <- GBC.getCallDepth
        Event.send eventChannel Event.Resumed >> singleStep
        callDepth1 <- GBC.getCallDepth
        if callDepth1 > callDepth0 then emulatorLoop Nothing (Just callDepth0) else pauseLoop
      StepOut -> do
        callDepth <- GBC.getCallDepth
        runEmulatorLoop Nothing (Just (callDepth - 1))
      Restart -> GBC.reset >> pauseLoop

  singleStep = GBC.step >> getCurrentAddress >>= updateDisassembly

  updateDisassembly address@(LongAddress _ pc) = do
    disassembly <- liftIO (Disassembly.get debugState)
    r           <- disassemblyRequired address disassembly
    when r $ do
      (disassembly', newLabels) <- disassembleFrom pc disassembly
      liftIO $ do
        Disassembly.set debugState disassembly'
        Labels.addFromList debugState eventChannel newLabels

  getCurrentAddress = do
    pc   <- readPC
    bank <- getBank pc
    pure (LongAddress bank pc)

  statisticsUpdate :: Int -> SystemTime -> ReaderT GBC.EmulatorState IO ()
  statisticsUpdate emulatorClock now = do
    emulatorClock' <- GBC.getEmulatorClock
    liftIO $ do
      now' <- getSystemTime
      Event.send
        eventChannel
        (Event.Statistics (systemTimeToDouble now' - systemTimeToDouble now)
                          (emulatorClock' - emulatorClock)
        )

  systemTimeToDouble (MkSystemTime s n) = fromIntegral s + (fromIntegral n / 1000000000)
