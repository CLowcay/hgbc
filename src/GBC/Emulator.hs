{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module GBC.Emulator
  ( EmulatorState(..)
  , initEmulatorState
  , isBreakFlagSet
  , clearBreakFlag
  , getEmulatorClock
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
import           GBC.Primitive.UnboxedRef
import           GBC.ROM
import           GBC.Registers
import           GBC.Timer
import           SDL.Orphans                    ( )
import qualified SDL

data EmulatorState = EmulatorState {
    mode            :: !EmulatorMode
  , memory          :: !Memory
  , vram            :: !VRAM
  , cpu             :: !CPUState
  , dmaState        :: !DMAState
  , graphicsState   :: !GraphicsState
  , graphicsSync    :: !GraphicsSync
  , keypadState     :: !KeypadState
  , timerState      :: !TimerState
  , audioState      :: !AudioState
  , breakFlag       :: !(IORef Bool)
  , hblankPending   :: !(IORef Bool) -- Set if there is an HBlank but we're not ready to do HBlank DMA yet
  , currentTime     :: !(UnboxedRef Int) -- Time in clocks
  , lastEventPoll   :: !(UnboxedRef Int) -- The time of the last event poll (in clocks)
}

instance HasMemory EmulatorState where
  {-# INLINE forMemory #-}
  forMemory = memory

instance HasCPU EmulatorState where
  {-# INLINE forCPUState #-}
  forCPUState = cpu

-- | Number of clocks to wait between polling for events.
pollDelay :: Int
pollDelay = 40000

-- | Create an initial bus state.
initEmulatorState :: ROM -> GraphicsSync -> Ptr Word8 -> IO EmulatorState
initEmulatorState rom graphicsSync frameBufferBytes = mdo
  let mode = case cgbSupport (romHeader rom) of
        CGBCompatible   -> CGB
        CGBExclusive    -> CGB
        CGBIncompatible -> DMG
  breakFlag     <- newIORef False
  vram          <- initVRAM mode

  portIF        <- newPort 0x00 0x1F alwaysUpdate
  portIE        <- newPort 0x00 0xFF alwaysUpdate

  cpu           <- initCPU portIF portIE mode (makeCatchupFunction emulatorState)
  dmaState      <- initDMA
  graphicsState <- initGraphics vram mode frameBufferBytes portIF
  keypadState   <- initKeypadState portIF
  timerState    <- initTimerState portIF
  audioState    <- initAudioState

  let allPorts =
        (IF, portIF)
          :  cpuPorts cpu
          ++ dmaPorts dmaState
          ++ graphicsPorts graphicsState
          ++ keypadPorts keypadState
          ++ timerPorts timerState
          ++ audioPorts audioState

  memory        <- initMemoryForROM rom vram allPorts portIE mode

  hblankPending <- newIORef False
  currentTime   <- newUnboxedRef 0
  lastEventPoll <- newUnboxedRef 0

  let emulatorState = EmulatorState { .. }
  pure emulatorState

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

pollEvents :: Int -> ReaderT EmulatorState IO ()
pollEvents now = do
  EmulatorState {..} <- ask
  lastPoll           <- liftIO $ readUnboxedRef lastEventPoll
  when (now - lastPoll > pollDelay) $ do
    handleEvents
    liftIO $ writeUnboxedRef lastEventPoll now

-- | Get the number of clocks since the emulator started.
getEmulatorClock :: ReaderT EmulatorState IO Int
getEmulatorClock = do
  EmulatorState {..} <- ask
  liftIO $ readUnboxedRef currentTime

-- | Execute a single step of the emulation.
step :: ReaderT EmulatorState IO ()
step = do
  EmulatorState {..} <- ask

  cycles             <- cpuStep
  now                <- liftIO $ readUnboxedRef currentTime
  pollEvents now

  cycleClocks <- getCPUCycleClocks
  let cpuClocks = cycles * cycleClocks

  graphicsEvent   <- updateHardware cycles cpuClocks
  dmaClockAdvance <- doPendingDMA dmaState

  liftIO $ writeUnboxedRef currentTime (now + cpuClocks + dmaClockAdvance)

  if dmaClockAdvance > 0
    then void $ updateHardware (dmaClockAdvance `div` cycleClocks) dmaClockAdvance
    else
      let doPendingHBlankDMA = do
            liftIO $ writeIORef hblankPending False
            hdmaClockAdvance <- doHBlankHDMA dmaState
            when (hdmaClockAdvance > 0) $ do
              void $ updateHardware (hdmaClockAdvance `div` cycleClocks) hdmaClockAdvance
              liftIO $ writeUnboxedRef currentTime (now + cpuClocks + hdmaClockAdvance)
      in  case graphicsEvent of
            NoGraphicsEvent -> do
              pending <- liftIO $ readIORef hblankPending
              when pending doPendingHBlankDMA
            HBlankEvent -> doPendingHBlankDMA

makeCatchupFunction :: EmulatorState -> Int -> Int -> IO ()
makeCatchupFunction emulatorState@EmulatorState {..} = \cycles clocksPerCycle ->
  let cpuClocks = cycles * clocksPerCycle
  in  do
        graphicsEvent <- runReaderT (updateHardware cycles cpuClocks) emulatorState
        when (graphicsEvent == HBlankEvent) $ writeIORef hblankPending True
        now <- readUnboxedRef currentTime
        writeUnboxedRef currentTime (now + cpuClocks)

updateHardware :: Int -> Int -> ReaderT EmulatorState IO GraphicsBusEvent
updateHardware cycles cpuClocks = do
  EmulatorState {..} <- ask
  liftIO $ do
    updateTimer timerState (cycles * 4)
    audioStep audioState cpuClocks
    graphicsStep graphicsState graphicsSync cpuClocks
