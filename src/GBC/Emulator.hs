{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Emulator
  ( EmulatorState(..)
  , initEmulatorState
  , isBreakFlagSet
  , clearBreakFlag
  , step
  , handleEvents
  )
where

import           Common
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           GBC.Audio
import           GBC.CPU
import           GBC.Graphics
import           GBC.Graphics.VRAM
import           GBC.Keypad
import           GBC.Memory
import           GBC.Mode
import           GBC.ROM
import           GBC.Registers
import           GBC.Timer
import           SDL.Orphans                    ( )
import qualified SDL

data EmulatorState = EmulatorState {
    lastEventPollAt :: !(IORef Word32)
  , hdmaSource      :: !(IORef Word16)
  , hdmaDestination :: !(IORef Word16)
  , hdmaActive      :: !(IORef Bool)
  , breakFlag       :: !(IORef Bool)
  , mode            :: !EmulatorMode
  , memory          :: !Memory
  , vram            :: !VRAM
  , cpu             :: !CPUState
  , graphicsState   :: !GraphicsState
  , graphicsSync    :: !GraphicsSync
  , keypadState     :: !KeypadState
  , timerState      :: !TimerState
  , audioState      :: !AudioState
}

instance HasMemory EmulatorState where
  {-# INLINE forMemory #-}
  forMemory = memory

instance HasCPU EmulatorState where
  {-# INLINE forCPUState #-}
  forCPUState = cpu

instance HasKeypad EmulatorState where
  {-# INLINE forKeypadState #-}
  forKeypadState = keypadState

instance HasTimer EmulatorState where
  {-# INLINE forTimerState #-}
  forTimerState = timerState

instance HasAudio EmulatorState where
  {-# INLINE forAudioState #-}
  forAudioState = audioState

instance HasGraphics EmulatorState where
  {-# INLINE forGraphicsState #-}
  forGraphicsState = graphicsState
  {-# INLINE forGraphicsSync #-}
  forGraphicsSync = graphicsSync

-- | Number of milliseconds to wait between polling for events.
pollDelay :: Word32
pollDelay = 10

-- | Create an initial bus state.
initEmulatorState :: ROM -> IO EmulatorState
initEmulatorState rom = do
  let mode = case cgbSupport (romHeader rom) of
        CGBCompatible   -> CGB
        CGBExclusive    -> CGB
        CGBIncompatible -> DMG
  lastEventPollAt <- newIORef 0
  breakFlag       <- newIORef False
  hdmaSource      <- newIORef 0
  hdmaDestination <- newIORef 0
  hdmaActive      <- newIORef False
  vram            <- initVRAM mode
  memory          <- initMemory rom vram mode
  cpu             <- initCPU mode
  graphicsSync    <- newGraphicsSync
  graphicsState   <- initGraphics vram
  keypadState     <- initKeypadState
  timerState      <- initTimerState
  audioState      <- initAudioState
  pure EmulatorState { .. }

-- | Check if the debug break flag is set.
isBreakFlagSet :: ReaderT EmulatorState IO Bool
isBreakFlagSet = liftIO . readIORef . breakFlag =<< ask

-- | Reset the debug break flag.
clearBreakFlag :: ReaderT EmulatorState IO ()
clearBreakFlag = liftIO . flip writeIORef False . breakFlag =<< ask

-- | Close all windows.
killWindows :: ReaderT EmulatorState IO ()
killWindows = do
  sync <- asks forGraphicsSync
  liftIO (putMVar (currentLine sync) 255)

-- | Poll and dispatch events.
handleEvents :: ReaderT EmulatorState IO ()
handleEvents = do
  events <- SDL.pollEvents
  keypadHandleUserEvents events
  for_ (SDL.eventPayload <$> events) $ \case
    (SDL.WindowClosedEvent _) -> killWindows
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ SDL.KeycodePause _)) ->
      liftIO . flip writeIORef True . breakFlag =<< ask
    _ -> pure ()

-- | Perform a DMA transfer.
doDMA :: HasMemory env => BusEvent -> ReaderT env IO ()
doDMA busEvent = when (DMA `elem` writeAddress busEvent) $ do
  address <- fromIntegral <$> readByte DMA
  dmaToOAM (address `shiftL` 8)

doGeneralHDMA :: Word8 -> ReaderT EmulatorState IO ()
doGeneralHDMA hdma5 = do
  hdma1 <- readByte HDMA1
  hdma2 <- readByte HDMA2
  hdma3 <- readByte HDMA3
  hdma4 <- readByte HDMA4
  writeByte HDMA5 0xFF
  go (makeHDMASource hdma1 hdma2) (makeHDMADestination hdma3 hdma4) (hdma5 + 1)
 where
  go _       _            0      = pure ()
  go !source !destination !count = do
    copy16 source destination
    go (source + 16) (destination + 16) (count - 1)

initHBlankHDMA :: Word8 -> ReaderT EmulatorState IO ()
initHBlankHDMA hdma5 = do
  EmulatorState {..} <- ask
  hdma1              <- readByte HDMA1
  hdma2              <- readByte HDMA2
  hdma3              <- readByte HDMA3
  hdma4              <- readByte HDMA4
  writeByte HDMA5 (hdma5 .&. 0x7F)
  liftIO $ do
    writeIORef hdmaSource $! makeHDMASource hdma1 hdma2
    writeIORef hdmaDestination $! makeHDMADestination hdma3 hdma4
    writeIORef hdmaActive True

doHBlankHDMA :: ReaderT EmulatorState IO Int
doHBlankHDMA = do
  EmulatorState {..} <- ask
  isActive           <- liftIO $ readIORef hdmaActive
  if not isActive
    then pure 0
    else do
      source      <- liftIO $ readIORef hdmaSource
      destination <- liftIO $ readIORef hdmaDestination
      liftIO $ do
        writeIORef hdmaSource $! source + 16
        writeIORef hdmaDestination $! destination + 16
      copy16 source destination

      hdma5 <- readByte HDMA5
      writeByte HDMA5 (hdma5 - 1)
      when (hdma5 == 0) $ liftIO $ writeIORef hdmaActive False

      pure 8

makeHDMASource :: Word8 -> Word8 -> Word16
makeHDMASource high low = (fromIntegral high `unsafeShiftL` 8) .|. (fromIntegral low .&. 0xF0)

makeHDMADestination :: Word8 -> Word8 -> Word16
makeHDMADestination high low =
  0x8000 + (((fromIntegral high .&. 0x1F) `unsafeShiftL` 8) .|. (fromIntegral low .&. 0xF0))

-- | Execute a single step of the emulation.
step :: ReaderT EmulatorState IO BusEvent
step = do
  EmulatorState {..} <- ask
  busEvent           <- cpuStep

  for_ (writeAddress busEvent) $ \case
    HDMA5 -> do
      hdma5 <- readByte HDMA5
      if hdma5 .&. 0x80 /= 0
        then initHBlankHDMA hdma5
        else do
          isActive <- liftIO $ readIORef hdmaActive
          if isActive
            then do
              liftIO $ writeIORef hdmaActive False
              writeByte HDMA5 (hdma5 .|. 0x80)
            else doGeneralHDMA hdma5

    _ -> pure ()

  keypadHandleBusEvent busEvent
  doDMA busEvent

  now      <- SDL.ticks
  lastPoll <- liftIO $ readIORef lastEventPollAt
  when (now - lastPoll > pollDelay) $ do
    handleEvents
    liftIO $ writeIORef lastEventPollAt =<< SDL.ticks

  key1 <- readByte KEY1
  let cpuClocks = clockAdvance busEvent
  updateTimer (if isFlagSet flagDoubleSpeed key1 then cpuClocks * 2 else cpuClocks)
  audioStep busEvent
  graphicsEvent <- graphicsStep busEvent

  case graphicsEvent of
    NoGraphicsEvent -> pure busEvent
    HBlankEvent     -> do
      dmaClockAdvance <- doHBlankHDMA
      when (dmaClockAdvance > 0) $ do
        updateTimer dmaClockAdvance
        let busEvent2 = BusEvent [] dmaClockAdvance ModeNormal
        audioStep busEvent2
        void $ graphicsStep busEvent2
      pure (busEvent { clockAdvance = clockAdvance busEvent + dmaClockAdvance })
