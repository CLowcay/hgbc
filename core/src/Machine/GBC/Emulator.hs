{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.Emulator
  ( State(..)
  , init
  , getClock
  , step
  , keyDown
  , keyUp
  , writeBgPalette
  , writeSpritePalette
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
import           Prelude                 hiding ( init )
import qualified Data.ByteString               as B
import qualified Data.Vector.Storable          as VS
import qualified Machine.GBC.Audio             as Audio
import qualified Machine.GBC.Bus               as Bus
import qualified Machine.GBC.CPU               as CPU
import qualified Machine.GBC.Color             as Color
import qualified Machine.GBC.DMA               as DMA
import qualified Machine.GBC.Graphics          as Graphics
import qualified Machine.GBC.Keypad            as Keypad
import qualified Machine.GBC.Memory            as Memory
import qualified Machine.GBC.Serial            as Serial
import qualified Machine.GBC.Timer             as Timer

data State = State {
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

instance Memory.Has State where
  {-# INLINE forState #-}
  forState = memory

instance CPU.Has State where
  {-# INLINE forState #-}
  forState = cpu

-- | Create a new 'State' given a 'ROM', a 'GraphicsSync', and a pointer
-- to the output frame buffer. The frame buffer is a 32bit RGB buffer with
-- 160x144 pixels.
init
  :: Maybe B.ByteString
  -> ROM
  -> Maybe EmulatorMode
  -> Color.Correction
  -> Serial.Sync
  -> Graphics.Sync
  -> Ptr Word8
  -> IO State
init bootROM rom requestedMode colorCorrection serialSync graphicsSync frameBufferBytes = mdo
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

  cpu           <- CPU.init portIF portIE modeRef
  dmaState      <- DMA.init vram modeRef
  graphicsState <- Graphics.init vram modeRef frameBufferBytes portIF
  keypadState   <- Keypad.init portIF
  audioState    <- Audio.init
  timerState    <- Timer.init (Audio.clockFrameSequencer audioState) (CPU.portKEY1 cpu) portIF
  serialState   <- Serial.init serialSync portIF modeRef

  let allPorts =
        (IF, portIF)
          :  CPU.ports cpu
          ++ DMA.ports dmaState
          ++ Graphics.ports graphicsState
          ++ Keypad.ports keypadState
          ++ Timer.ports timerState
          ++ Audio.ports audioState
          ++ Serial.ports serialState

  memory <- Memory.initForROM (VS.fromList . B.unpack <$> bootROM) rom vram allPorts portIE modeRef

  hblankPending <- newIORef False
  currentTime   <- newUnboxedRef 0
  lastEventPoll <- newUnboxedRef 0

  let emulatorState = State { .. }
  pure emulatorState

instance Bus.Has State where
  read address = do
    clocks <- CPU.getCycleClocks
    updateTime clocks
    Memory.readByte address <* updateHardware clocks
  write address value = do
    clocks <- CPU.getCycleClocks
    updateTime clocks
    Memory.writeByte address value
    updateHardware clocks
  delay = do
    clocks <- CPU.getCycleClocks
    updateTime clocks
    updateHardware clocks
  delayClocks n = do
    clocks <- CPU.getCycleClocks
    updateTime n
    replicateM_ (n `div` clocks) (updateHardware clocks)

{-# INLINE updateTime #-}
updateTime :: Int -> ReaderT State IO ()
updateTime clocks = do
  time <- asks currentTime
  now  <- readUnboxedRef time
  writeUnboxedRef time (now + clocks)

-- | Get the number of clocks since the emulator started.
{-# INLINABLE getClock #-}
getClock :: ReaderT State IO Int
getClock = readUnboxedRef =<< asks currentTime

-- | Execute one CPU instruction and update all of the emulated hardware
-- accordingly. This may cause the audio queue to fill up, or it may trigger a
-- request to flip the frame buffer.
step :: ReaderT State IO ()
step = do
  CPU.step

  state <- ask
  DMA.doPendingHDMA (dmaState state)
  isHBlankPending <- liftIO $ readIORef (hblankPending state)
  when isHBlankPending $ do
    liftIO $ writeIORef (hblankPending state) False
    DMA.doHBlankHDMA (dmaState state)

updateHardware :: Int -> ReaderT State IO ()
updateHardware clocksPerCycle = do
  State {..} <- ask
  DMA.update dmaState
  liftIO $ do
    Serial.update serialState
    Timer.update timerState
    Audio.step audioState clocksPerCycle
    graphicsEvent <- Graphics.step graphicsState graphicsSync clocksPerCycle
    when (graphicsEvent == Graphics.HBlankEvent) $ writeIORef hblankPending True

-- | Notify that a key is pressed.
keyDown :: State -> Keypad.Key -> IO ()
keyDown state = Keypad.press (keypadState state)

-- | Notify that a key is released.
keyUp :: State -> Keypad.Key -> IO ()
keyUp state = Keypad.release (keypadState state)

-- | Set a background palette.
writeBgPalette :: State -> Int -> (Word32, Word32, Word32, Word32) -> IO ()
writeBgPalette state = writeRGBPalette (vram state) False

-- | Set a sprite palette.
writeSpritePalette :: State -> Int -> (Word32, Word32, Word32, Word32) -> IO ()
writeSpritePalette state = writeRGBPalette (vram state) True
