{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.Audio.PulseChannel
  ( PulseChannel,
    newPulseChannel,
  )
where

import Control.Monad.Reader (unless, when)
import Data.Bits (Bits (complement, (.&.), (.|.)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Machine.GBC.Audio.Common (Channel (..), FrameSequencerOutput, flagLength, flagTrigger, getFrequency, isEnvelopeClockingStep, isLengthClockingStep, isSweepClockingStep, newAudioPort, newAudioPortWithReadMask, updateStatus)
import Machine.GBC.Audio.Envelope (Envelope, clockEnvelope, envelopeVolume, initEnvelope, newEnvelope)
import Machine.GBC.Audio.Length (Length, clockLength, extraClocks, initLength, newLength, powerOffLength, reloadLength)
import Machine.GBC.Audio.Sweep (Sweep, clockSweep, flagNegate, hasPerformedSweepCalculationInNegateMode, initSweep, newSweep)
import Machine.GBC.Primitive.Counter (Counter)
import qualified Machine.GBC.Primitive.Counter as Counter
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port
import Machine.GBC.Primitive.StateCycle (StateCycle)
import qualified Machine.GBC.Primitive.StateCycle as StateCycle
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import Machine.GBC.Util (isFlagSet, (.>>.))

data PulseChannel = PulseChannel
  { output :: !(UnboxedRef Int),
    enable :: !(IORef Bool),
    dacEnable :: !(IORef Bool),
    port0 :: !Port,
    port1 :: !Port,
    port2 :: !Port,
    port3 :: !Port,
    port4 :: !Port,
    port52 :: !Port,
    hasSweepUnit :: !Bool,
    channelEnabledFlag :: !Word8,
    sweepUnit :: !Sweep,
    frequencyCounter :: !Counter,
    dutyCycle :: !(UnboxedRef Int),
    envelope :: !Envelope,
    lengthCounter :: !Length
  }

newPulseChannel :: Bool -> Port -> StateCycle FrameSequencerOutput -> Word8 -> IO PulseChannel
newPulseChannel hasSweepUnit port52 frameSequencer channelEnabledFlag = mdo
  output <- newUnboxedRef 0
  enable <- newIORef False
  dacEnable <- newIORef True

  let port0ReadMask = if hasSweepUnit then 0x80 else 0xFF
  let port0WriteMask = if hasSweepUnit then 0x7F else 0x00
  port0 <- newAudioPortWithReadMask port52 0xFF port0ReadMask port0WriteMask $ \_ register0 -> do
    when hasSweepUnit $ do
      hasNegated <- hasPerformedSweepCalculationInNegateMode sweepUnit
      when (hasNegated && not (isFlagSet flagNegate register0)) $
        disableIO port52 channelEnabledFlag output enable
    pure register0

  port1 <- newAudioPortWithReadMask port52 0xFF 0x3F 0xFF $ \_ register1 -> do
    reloadLength lengthCounter register1
    pure register1

  port2 <- newAudioPort port52 0xFF 0xFF $ \_ register2 -> do
    let isDacEnabled = register2 .&. 0xF8 /= 0
    unless isDacEnabled $ disableIO port52 channelEnabledFlag output enable
    writeIORef dacEnable isDacEnabled
    pure register2

  port3 <- newAudioPortWithReadMask port52 0xFF 0xFF 0xFF Port.alwaysUpdate

  port4 <- newAudioPortWithReadMask port52 0xFF 0xBF 0xC7 $ \previous register4 -> do
    frame <- StateCycle.getState frameSequencer
    when
      (isFlagSet flagLength register4 && not (isFlagSet flagLength previous))
      (extraClocks lengthCounter frame (disableIO port52 channelEnabledFlag output enable))

    when (isFlagSet flagTrigger register4) $ do
      register0 <- Port.readDirect port0
      register2 <- Port.readDirect port2
      register3 <- Port.readDirect port3
      let frequency = getFrequency register3 register4

      isDacEnabled <- readIORef dacEnable
      writeIORef enable isDacEnabled
      updateStatus port52 channelEnabledFlag isDacEnabled
      when hasSweepUnit $
        initSweep
          sweepUnit
          frequency
          register0
          (disableIO port52 channelEnabledFlag output enable)
      initLength lengthCounter frame (isFlagSet flagLength register4)
      initEnvelope envelope register2 frame

      -- Quirk: When triggering a pulse channel, the low two bits of the
      -- frequency counter are not modified.
      count0 <- Counter.get frequencyCounter
      let count1 = 6 + getTimerPeriod frequency
      Counter.reload frequencyCounter ((count0 .&. 3) .|. (count1 .&. complement 3))

    pure register4

  sweepUnit <- newSweep port3 port4
  frequencyCounter <- Counter.new 0x7FF
  dutyCycle <- newUnboxedRef 0
  envelope <- newEnvelope
  lengthCounter <- newLength 0x3F
  pure PulseChannel {..}

disableIO :: Port -> Word8 -> UnboxedRef Int -> IORef Bool -> IO ()
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
    Port.writeDirect port0 0
    Port.writeDirect port1 0
    Port.write port2 0
    Port.writeDirect port3 0
    Port.writeDirect port4 0
    powerOffLength lengthCounter
    writeUnboxedRef dutyCycle 0

  frameSequencerClock channel@PulseChannel {..} step = do
    register4 <- Port.readDirect port4
    when (isLengthClockingStep step && isFlagSet flagLength register4) $
      clockLength lengthCounter $
        disable channel
    when (isEnvelopeClockingStep step) $ clockEnvelope envelope
    when (hasSweepUnit && isSweepClockingStep step) $ do
      register0 <- Port.readDirect port0
      clockSweep sweepUnit register0 (disable channel)

  masterClock PulseChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $
      Counter.update frequencyCounter clockAdvance $ do
        i0 <- readUnboxedRef dutyCycle
        let i = (i0 + 1) .&. 7
        writeUnboxedRef dutyCycle i

        register1 <- Port.readDirect port1
        sample <- envelopeVolume envelope
        writeUnboxedRef output (if dutyCycleOutput register1 i then sample else 0)

        register3 <- Port.readDirect port3
        register4 <- Port.readDirect port4
        pure (getTimerPeriod (getFrequency register3 register4))

  directReadPorts PulseChannel {..} =
    (,,,,)
      <$> Port.readDirect port0
      <*> Port.readDirect port1
      <*> Port.readDirect port2
      <*> Port.readDirect port3
      <*> Port.readDirect port4

{-# INLINE dutyCycleOutput #-}
dutyCycleOutput :: Word8 -> Int -> Bool
dutyCycleOutput register1 i = case register1 .>>. 6 of
  0 -> i == 0
  1 -> i <= 1
  2 -> i <= 1 || i >= 6
  3 -> i > 1
  x -> error ("Invalid duty cycle " <> show x)

getTimerPeriod :: Int -> Int
getTimerPeriod f = 4 * (2048 - f)
