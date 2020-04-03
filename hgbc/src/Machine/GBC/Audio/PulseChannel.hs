{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Machine.GBC.Audio.PulseChannel
  ( PulseChannel
  , newPulseChannel
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.Audio.Common
import           Machine.GBC.Audio.Envelope
import           Machine.GBC.Audio.Length
import           Machine.GBC.Audio.Sweep
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Util

data PulseChannel = PulseChannel {
    output             :: !(UnboxedRef Int)
  , enable             :: !(IORef Bool)
  , dacEnable          :: !(IORef Bool)
  , port0              :: !(Port Word8)
  , port1              :: !(Port Word8)
  , port2              :: !(Port Word8)
  , port3              :: !(Port Word8)
  , port4              :: !(Port Word8)
  , port52             :: !(Port Word8)
  , hasSweepUnit       :: !Bool
  , channelEnabledFlag :: !Word8
  , sweepUnit          :: !Sweep
  , frequencyCounter   :: !Counter
  , dutyCycle          :: !(UnboxedRef Int)
  , envelope           :: !Envelope
  , lengthCounter      :: !Length
}

newPulseChannel :: Bool -> Port Word8 -> StateCycle FrameSequencerOutput -> Word8 -> IO PulseChannel
newPulseChannel hasSweepUnit port52 frameSequencer channelEnabledFlag = mdo
  output    <- newUnboxedRef 0
  enable    <- newIORef False
  dacEnable <- newIORef True

  let port0ReadMask  = if hasSweepUnit then 0x80 else 0xFF
  let port0WriteMask = if hasSweepUnit then 0x7F else 0x00
  port0 <- newAudioPortWithReadMask port52 0xFF port0ReadMask port0WriteMask $ \_ register0 -> do
    when hasSweepUnit $ do
      hasNegated <- hasPerformedSweepCalculationInNegateMode sweepUnit
      when (hasNegated && not (isFlagSet flagNegate register0))
        $ disableIO port52 channelEnabledFlag output enable
    pure register0

  port1 <- newAudioPortWithReadMask port52 0xFF 0x3F 0xFF $ \_ register1 -> do
    reloadLength lengthCounter register1
    pure register1

  port2 <- newAudioPort port52 0xFF 0xFF $ \_ register2 -> do
    let isDacEnabled = register2 .&. 0xF8 /= 0
    unless isDacEnabled $ disableIO port52 channelEnabledFlag output enable
    writeIORef dacEnable isDacEnabled
    pure register2

  port3 <- newAudioPortWithReadMask port52 0xFF 0xFF 0xFF alwaysUpdate

  port4 <- newAudioPortWithReadMask port52 0xFF 0xBF 0xC7 $ \previous register4 -> do
    frame <- getStateCycle frameSequencer
    when (isFlagSet flagLength register4 && not (isFlagSet flagLength previous))
         (extraClocks lengthCounter frame (disableIO port52 channelEnabledFlag output enable))

    when (isFlagSet flagTrigger register4) $ do
      register0 <- directReadPort port0
      register2 <- directReadPort port2
      register3 <- directReadPort port3
      let frequency = getFrequency register3 register4

      isDacEnabled <- readIORef dacEnable
      writeIORef enable isDacEnabled
      updateStatus port52 channelEnabledFlag isDacEnabled
      when hasSweepUnit $ initSweep sweepUnit
                                    frequency
                                    register0
                                    (disableIO port52 channelEnabledFlag output enable)
      initLength lengthCounter frame (isFlagSet flagLength register4)
      initEnvelope envelope register2 frame

      -- Quirk: When triggering a pulse channel, the low two bits of the
      -- frequency counter are not modified.
      count0 <- getCounter frequencyCounter
      let count1 = 6 + getTimerPeriod frequency
      reloadCounter frequencyCounter ((count0 .&. 3) .|. (count1 .&. complement 3))

    pure register4

  sweepUnit        <- newSweep port3 port4
  frequencyCounter <- newCounter 0x7FF
  dutyCycle        <- newUnboxedRef 0
  envelope         <- newEnvelope
  lengthCounter    <- newLength 0x3F
  pure PulseChannel { .. }

disableIO :: Port Word8 -> Word8 -> UnboxedRef Int -> IORef Bool -> IO ()
disableIO port52 channelEnabledFlag output enable = do
  writeUnboxedRef output 0
  writeIORef enable False
  updateStatus port52 channelEnabledFlag False

instance Channel PulseChannel where
  getOutput PulseChannel {..} = readUnboxedRef output
  disable PulseChannel {..} = disableIO port52 channelEnabledFlag output enable
  getStatus PulseChannel {..} = readIORef enable
  getPorts PulseChannel {..} = [(0, port0), (1, port1), (2, port2), (3, port3), (4, port4)]

  powerOff PulseChannel {..} = do
    directWritePort port0 0
    directWritePort port1 0
    writePort port2 0
    directWritePort port3 0
    directWritePort port4 0
    powerOffLength lengthCounter
    writeUnboxedRef dutyCycle 0

  frameSequencerClock channel@PulseChannel {..} step = do
    register4 <- directReadPort port4
    when (isLengthClockingStep step && isFlagSet flagLength register4)
      $ clockLength lengthCounter
      $ disable channel
    when (isEnvelopeClockingStep step) $ clockEnvelope envelope
    when (hasSweepUnit && isSweepClockingStep step) $ do
      register0 <- directReadPort port0
      clockSweep sweepUnit register0 (disable channel)

  masterClock PulseChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $ do
      reloads <- updateReloadingCounter frequencyCounter clockAdvance $ do
        register3 <- directReadPort port3
        register4 <- directReadPort port4
        pure (getTimerPeriod (getFrequency register3 register4))

      i0 <- readUnboxedRef dutyCycle
      let i = (i0 + reloads) .&. 7
      writeUnboxedRef dutyCycle i

      register1 <- directReadPort port1
      sample    <- envelopeVolume envelope
      writeUnboxedRef output (if dutyCycleOutput register1 i then sample else 0)

dutyCycleOutput :: Word8 -> Int -> Bool
dutyCycleOutput register1 i = case register1 .>>. 6 of
  0 -> i == 0
  1 -> i <= 1
  2 -> i <= 1 || i >= 6
  3 -> i > 1
  x -> error ("Invalid duty cycle " <> show x)

getTimerPeriod :: Int -> Int
getTimerPeriod f = 4 * (2048 - f)
