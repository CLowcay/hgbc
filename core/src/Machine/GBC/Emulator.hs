{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.Emulator
  ( EmulatorState(..)
  , initEmulatorState
  , getEmulatorClock
  , step
  )
where

import           Control.Monad.Reader
import           Control.Applicative
import           Data.IORef
import           Data.Word
import           Foreign.Ptr
import           Machine.GBC.Audio
import           Machine.GBC.CPU
import           Machine.GBC.DMA
import           Machine.GBC.Graphics
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Keypad
import           Machine.GBC.Memory
import           Machine.GBC.Mode
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.ROM
import           Machine.GBC.Registers
import           Machine.GBC.Timer
import qualified Data.ByteString               as B
import qualified Data.Vector.Storable          as VS
import           Data.Functor
import           Data.Maybe

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

-- | Create a new 'EmulatorState' given a 'ROM', a 'GraphicsSync', and a pointer
-- to the output frame buffer. The frame buffer is a 32bit RGB buffer with
-- 160x144 pixels.
initEmulatorState
  :: Maybe B.ByteString
  -> ROM
  -> Maybe EmulatorMode
  -> ColorCorrection
  -> GraphicsSync
  -> Ptr Word8
  -> IO EmulatorState
initEmulatorState bootROM rom requestedMode colorCorrection graphicsSync frameBufferBytes = mdo
  let bootMode = bootROM <&> \content -> if B.length content > 0x100 then CGB else DMG
  let romMode = case cgbSupport (romHeader rom) of
        CGBCompatible   -> CGB
        CGBExclusive    -> CGB
        CGBIncompatible -> DMG
  let mode = fromMaybe romMode (requestedMode <|> bootMode)
  vram <- initVRAM colorCorrection

  writeRGBPalette vram False 0 (0xffffffff, 0xaaaaaaff, 0x555555ff, 0x000000ff)
  writeRGBPalette vram True  0 (0xffffffff, 0xaaaaaaff, 0x555555ff, 0x000000ff)
  writeRGBPalette vram True  1 (0xffffffff, 0xaaaaaaff, 0x555555ff, 0x000000ff)

  modeRef       <- newIORef mode
  portIF        <- newPort 0xE0 0x1F alwaysUpdate
  portIE        <- newPort 0x00 0xFF alwaysUpdate

  cpu           <- initCPU portIF portIE mode (makeCatchupFunction emulatorState)
  dmaState      <- initDMA
  graphicsState <- initGraphics vram modeRef frameBufferBytes portIF
  keypadState   <- initKeypadState portIF
  audioState    <- initAudioState
  timerState    <- initTimerState (clockFrameSequencer audioState) (portKEY1 cpu) portIF

  let allPorts =
        (IF, portIF)
          :  cpuPorts cpu
          ++ dmaPorts dmaState
          ++ graphicsPorts graphicsState
          ++ keypadPorts keypadState
          ++ timerPorts timerState
          ++ audioPorts audioState

  memory <- initMemoryForROM (VS.fromList . B.unpack <$> bootROM) rom vram allPorts portIE modeRef

  hblankPending <- newIORef False
  currentTime   <- newUnboxedRef 0
  lastEventPoll <- newUnboxedRef 0

  let emulatorState = EmulatorState { .. }
  pure emulatorState

-- | Get the number of clocks since the emulator started.
getEmulatorClock :: ReaderT EmulatorState IO Int
getEmulatorClock = do
  EmulatorState {..} <- ask
  liftIO $ readUnboxedRef currentTime

-- | Execute one CPU instruction and update all of the emulated hardware
-- accordingly. This may cause the audio queue to fill up, or it may trigger a
-- request to flip the frame buffer.
step :: ReaderT EmulatorState IO ()
step = do
  EmulatorState {..} <- ask
  cycles             <- cpuStep
  now                <- liftIO $ readUnboxedRef currentTime

  cycleClocks        <- getCPUCycleClocks
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
makeCatchupFunction emulatorState@EmulatorState {..} cycles clocksPerCycle =
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
    updateTimer timerState cycles
    audioStep audioState cpuClocks
    graphicsStep graphicsState graphicsSync cpuClocks