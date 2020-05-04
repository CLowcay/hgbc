{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.Emulator
  ( EmulatorState(..)
  , initEmulatorState
  , getEmulatorClock
  , step
  )
where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Data.Word
import           Foreign.Ptr
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Mode
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.ROM
import           Machine.GBC.Registers
import qualified Data.ByteString               as B
import qualified Data.Vector.Storable          as VS
import qualified Machine.GBC.Audio             as Audio
import qualified Machine.GBC.CPU               as CPU
import qualified Machine.GBC.DMA               as DMA
import qualified Machine.GBC.Graphics          as Graphics
import qualified Machine.GBC.Keypad            as Keypad
import qualified Machine.GBC.Memory            as Memory
import qualified Machine.GBC.Serial            as Serial
import qualified Machine.GBC.Timer             as Timer

data EmulatorState = EmulatorState {
    mode            :: !EmulatorMode
  , memory          :: !Memory.State
  , vram            :: !VRAM
  , cpu             :: !CPU.State
  , dmaState        :: !DMA.State
  , graphicsState   :: !Graphics.State
  , graphicsSync    :: !Graphics.Sync
  , keypadState     :: !Keypad.State
  , timerState      :: !Timer.State
  , audioState      :: !Audio.State
  , serialState     :: !Serial.State
  , hblankPending   :: !(IORef Bool) -- Set if there is an HBlank but we're not ready to do HBlank DMA yet
  , currentTime     :: !(UnboxedRef Int) -- Time in clocks
  , lastEventPoll   :: !(UnboxedRef Int) -- The time of the last event poll (in clocks)
}

instance Memory.Has EmulatorState where
  {-# INLINE forState #-}
  forState = memory

instance CPU.Has EmulatorState where
  {-# INLINE forState #-}
  forState = cpu

-- | Create a new 'EmulatorState' given a 'ROM', a 'GraphicsSync', and a pointer
-- to the output frame buffer. The frame buffer is a 32bit RGB buffer with
-- 160x144 pixels.
initEmulatorState
  :: Maybe B.ByteString
  -> ROM
  -> Maybe EmulatorMode
  -> ColorCorrection
  -> Serial.Sync
  -> Graphics.Sync
  -> Ptr Word8
  -> IO EmulatorState
initEmulatorState bootROM rom requestedMode colorCorrection serialSync graphicsSync frameBufferBytes
  = mdo
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

    cpu           <- CPU.init portIF portIE mode (makeCatchupFunction emulatorState)
    dmaState      <- DMA.init
    graphicsState <- Graphics.init vram modeRef frameBufferBytes portIF
    keypadState   <- Keypad.init portIF
    audioState    <- Audio.init
    timerState    <- Timer.init (Audio.clockFrameSequencer audioState) (CPU.portKEY1 cpu) portIF
    serialState   <- Serial.init serialSync portIF

    let allPorts =
          (IF, portIF)
            :  CPU.ports cpu
            ++ DMA.ports dmaState
            ++ Graphics.ports graphicsState
            ++ Keypad.ports keypadState
            ++ Timer.ports timerState
            ++ Audio.ports audioState
            ++ Serial.ports serialState

    memory <- Memory.initForROM (VS.fromList . B.unpack <$> bootROM)
                                rom
                                vram
                                allPorts
                                portIE
                                modeRef

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
  cycles             <- CPU.step
  now                <- liftIO $ readUnboxedRef currentTime

  cycleClocks        <- CPU.getCycleClocks
  let cpuClocks = cycles * cycleClocks

  graphicsEvent   <- updateHardware cycles cpuClocks
  dmaClockAdvance <- DMA.doPendingHDMA dmaState

  liftIO $ writeUnboxedRef currentTime (now + cpuClocks + dmaClockAdvance)

  if dmaClockAdvance > 0
    then void $ updateHardware (dmaClockAdvance `div` cycleClocks) dmaClockAdvance
    else
      let doPendingHBlankDMA = do
            liftIO $ writeIORef hblankPending False
            hdmaClockAdvance <- DMA.doHBlankHDMA dmaState
            when (hdmaClockAdvance > 0) $ do
              void $ updateHardware (hdmaClockAdvance `div` cycleClocks) hdmaClockAdvance
              liftIO $ writeUnboxedRef currentTime (now + cpuClocks + hdmaClockAdvance)
      in  case graphicsEvent of
            Graphics.NoGraphicsEvent -> do
              pending <- liftIO $ readIORef hblankPending
              when pending doPendingHBlankDMA
            Graphics.HBlankEvent -> doPendingHBlankDMA

makeCatchupFunction :: EmulatorState -> Int -> Int -> IO ()
makeCatchupFunction emulatorState@EmulatorState {..} cycles clocksPerCycle =
  let cpuClocks = cycles * clocksPerCycle
  in  do
        graphicsEvent <- runReaderT (updateHardware cycles cpuClocks) emulatorState
        when (graphicsEvent == Graphics.HBlankEvent) $ writeIORef hblankPending True
        now <- readUnboxedRef currentTime
        writeUnboxedRef currentTime (now + cpuClocks)

updateHardware :: Int -> Int -> ReaderT EmulatorState IO Graphics.BusEvent
updateHardware cycles cpuClocks = do
  EmulatorState {..} <- ask
  DMA.update dmaState cycles
  liftIO $ do
    Serial.update serialState cycles
    Timer.update timerState cycles
    Audio.step audioState cpuClocks
    Graphics.step graphicsState graphicsSync cpuClocks
