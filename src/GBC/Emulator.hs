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
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           Foreign.Ptr
import           GBC.Audio
import           GBC.CPU
import           GBC.DMA
import           GBC.Graphics
import           GBC.Graphics.VRAM
import           GBC.Keypad
import           GBC.Memory
import           GBC.Mode
import           GBC.Primitive
import           GBC.ROM
import           GBC.Registers
import           GBC.Timer
import           SDL.Orphans                    ( )
import qualified SDL

data EmulatorState = EmulatorState {
    lastEventPollAt :: !(IORef Word32)
  , breakFlag       :: !(IORef Bool)
  , mode            :: !EmulatorMode
  , memory          :: !Memory
  , vram            :: !VRAM
  , cpu             :: !CPUState
  , dmaState        :: !DMAState
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

-- | Number of milliseconds to wait between polling for events.
pollDelay :: Word32
pollDelay = 10

-- | Create an initial bus state.
initEmulatorState :: ROM -> GraphicsSync -> Ptr Word8 -> IO EmulatorState
initEmulatorState rom graphicsSync frameBufferBytes = do
  let mode = case cgbSupport (romHeader rom) of
        CGBCompatible   -> CGB
        CGBExclusive    -> CGB
        CGBIncompatible -> DMG
  lastEventPollAt <- newIORef 0
  breakFlag       <- newIORef False
  vram            <- initVRAM mode

  portIF          <- newPort 0x00 0x1F alwaysUpdate
  portIE          <- newPort 0x00 0xFF alwaysUpdate

  cpu             <- initCPU portIF portIE mode
  dmaState        <- initDMA
  graphicsState   <- initGraphics vram mode frameBufferBytes portIF
  keypadState     <- initKeypadState portIF
  timerState      <- initTimerState portIF
  audioState      <- initAudioState

  let allPorts =
        (IF, portIF)
          :  cpuPorts cpu
          ++ dmaPorts dmaState
          ++ graphicsPorts graphicsState
          ++ keypadPorts keypadState
          ++ timerPorts timerState
          ++ audioPorts audioState

  memory <- initMemory rom vram allPorts portIE mode

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
  sync <- asks graphicsSync
  liftIO (putMVar (signalWindow sync) Quit)
  pure ()

-- | Poll and dispatch events.
handleEvents :: ReaderT EmulatorState IO ()
handleEvents = do
  events      <- SDL.pollEvents
  keypadState <- asks keypadState
  liftIO $ keypadHandleUserEvents keypadState events
  for_ (SDL.eventPayload <$> events) $ \case
    (SDL.WindowClosedEvent _) -> killWindows
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ SDL.KeycodeK _)) ->
      liftIO . flip writeIORef True . breakFlag =<< ask
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ SDL.KeycodePause _)) ->
      liftIO . flip writeIORef True . breakFlag =<< ask
    _ -> pure ()

pollEvents :: ReaderT EmulatorState IO ()
pollEvents = do
  EmulatorState {..} <- ask
  now                <- SDL.ticks
  lastPoll           <- liftIO $ readIORef lastEventPollAt
  when (now - lastPoll > pollDelay) $ do
    handleEvents
    liftIO $ writeIORef lastEventPollAt =<< SDL.ticks

-- | Execute a single step of the emulation.
step :: ReaderT EmulatorState IO Int
step = do
  EmulatorState {..} <- ask
  cycles             <- cpuStep
  pollEvents

  cycleClocks <- getCPUCycleClocks
  let cpuClocks = cycles * cycleClocks

  graphicsEvent   <- updateHardware cycles cycleClocks

  dmaClockAdvance <- doPendingDMA dmaState
  if dmaClockAdvance > 0
    then do
      void $ updateHardware (dmaClockAdvance `div` cycleClocks) cycleClocks
      pure (cpuClocks + dmaClockAdvance)
    else case graphicsEvent of
      NoGraphicsEvent -> pure cpuClocks
      HBlankEvent     -> do
        hdmaClockAdvance <- doHBlankHDMA dmaState
        when (hdmaClockAdvance > 0) $ void $ updateHardware (hdmaClockAdvance `div` cycleClocks)
                                                            cycleClocks
        pure (cpuClocks + hdmaClockAdvance)

updateHardware :: Int -> Int -> ReaderT EmulatorState IO GraphicsBusEvent
updateHardware cycles clocksPerCycle = do
  EmulatorState {..} <- ask
  liftIO $ do
    updateTimer timerState (cycles * 4)
    audioStep audioState clocks
    graphicsStep graphicsState graphicsSync clocks
  where clocks = cycles * clocksPerCycle
