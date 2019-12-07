{-# LANGUAGE LambdaCase #-}
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

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           GBC.Audio
import           GBC.CPU
import           GBC.ROM
import           GBC.Graphics
import           GBC.Graphics.VRAM
import           GBC.Mode
import           GBC.Keypad
import           GBC.Memory
import           GBC.Registers
import           GBC.Timer
import           SDL.Orphans                    ( )
import qualified SDL

data EmulatorState = EmulatorState {
    lastEventPollAt :: !(IORef Word32)
  , breakFlag       :: !(IORef Bool)
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
  vram            <- initVRAM mode
  memory          <- initMemory rom vram
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

-- | Execute a single step of the emulation.
step :: ReaderT EmulatorState IO BusEvent
step = do
  busEvent <- cpuStep
  keypadHandleBusEvent busEvent
  doDMA busEvent

  EmulatorState {..} <- ask
  now                <- SDL.ticks
  lastPoll           <- liftIO $ readIORef lastEventPollAt
  when (now - lastPoll > pollDelay) $ do
    handleEvents
    liftIO $ writeIORef lastEventPollAt =<< SDL.ticks

  graphicsStep busEvent
  updateTimer (clockAdvance busEvent)
  audioStep busEvent
  pure busEvent
