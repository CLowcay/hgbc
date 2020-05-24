{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.Audio.NoiseChannel where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.Audio.Common
import           Machine.GBC.Audio.Envelope
import           Machine.GBC.Audio.Length
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Util

data NoiseChannel = NoiseChannel {
    enable           :: !(IORef Bool)
  , dacEnable        :: !(IORef Bool)
  , output           :: !(UnboxedRef Int)
  , port1            :: !Port
  , port2            :: !Port
  , port3            :: !Port
  , port4            :: !Port
  , port52           :: !Port
  , lengthCounter    :: !Length
  , envelope         :: !Envelope
  , frequencyCounter :: !Counter
  , lfsr             :: !(LinearFeedbackShiftRegister Word16)
}

newNoiseChannel :: Port -> StateCycle FrameSequencerOutput -> IO NoiseChannel
newNoiseChannel port52 frameSequencer = mdo
  enable    <- newIORef False
  dacEnable <- newIORef True
  output    <- newUnboxedRef 0

  port1     <- newAudioPortWithReadMask port52 0xFF 0xFF 0x3F $ \_ register1 -> do
    reloadLength lengthCounter register1
    pure register1

  port2 <- newAudioPortWithReadMask port52 0xFF 0x00 0xFF $ \_ register2 -> do
    let isDacEnabled = register2 .&. 0xF8 /= 0
    unless isDacEnabled $ disableIO port52 output enable
    writeIORef dacEnable isDacEnabled
    pure register2

  port3 <- newAudioPortWithReadMask port52 0xFF 0x00 0xFF alwaysUpdate

  port4 <- newAudioPortWithReadMask port52 0xFF 0xBF 0xC0 $ \previous register4 -> do
    frame <- getStateCycle frameSequencer
    when (isFlagSet flagLength register4 && not (isFlagSet flagLength previous))
         (extraClocks lengthCounter frame (disableIO port52 output enable))

    when (isFlagSet flagTrigger register4) $ do
      register2    <- directReadPort port2
      register3    <- directReadPort port3
      isDacEnabled <- readIORef dacEnable
      writeIORef enable isDacEnabled
      updateStatus port52 flagChannel4Enable isDacEnabled
      initLength lengthCounter frame (isFlagSet flagLength register4)
      initEnvelope envelope register2 frame
      initLinearFeedbackShiftRegister 0xFFFF lfsr
      reloadCounter frequencyCounter (6 + timerPeriod register3)
    pure register4

  lengthCounter    <- newLength 0x3F
  envelope         <- newEnvelope
  frequencyCounter <- newCounter 0x7FF
  lfsr             <- newLinearFeedbackShiftRegister
  pure NoiseChannel { .. }

disableIO :: Port -> UnboxedRef Int -> IORef Bool -> IO ()
disableIO port52 output enable = do
  writeUnboxedRef output 0
  writeIORef enable False
  updateStatus port52 flagChannel4Enable False

instance Channel NoiseChannel where
  getOutput NoiseChannel {..} = readUnboxedRef output
  disable NoiseChannel {..} = disableIO port52 output enable
  getStatus NoiseChannel {..} = readIORef enable
  getPorts NoiseChannel {..} = [(1, port1), (2, port2), (3, port3), (4, port4)]

  powerOff NoiseChannel {..} = do
    directWritePort port1 0
    writePort port2 0
    directWritePort port3 0
    directWritePort port4 0
    powerOffLength lengthCounter

  frameSequencerClock NoiseChannel {..} step = do
    register4 <- directReadPort port4
    when (isFlagSet flagLength register4 && isLengthClockingStep step)
      $ clockLength lengthCounter (disableIO port52 output enable)
    when (isEnvelopeClockingStep step) $ clockEnvelope envelope

  masterClock NoiseChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      register3   <- directReadPort port3
      -- Quirk: If the shift clock is 14 or 15, then do not clock the LFSR.
      registerOut <- if shiftClock register3 >= 14
        then currentBit lfsr
        else nextBit lfsr (widthMask register3)
      sample <- envelopeVolume envelope
      writeUnboxedRef output (if registerOut .&. 1 == 0 then sample else 0)
      pure (timerPeriod register3)

  directReadPorts NoiseChannel {..} =
    (0, , , , )
      <$> directReadPort port1
      <*> directReadPort port2
      <*> directReadPort port3
      <*> directReadPort port4

widthMask :: Word8 -> Word16
widthMask register3 = if register3 `testBit` 3 then 0x0040 else 0x4000

shiftClock :: Word8 -> Int
shiftClock register3 = fromIntegral register3 .>>. 4

ratio :: Word8 -> Int
ratio register3 = fromIntegral register3 .&. 0x07

timerPeriod :: Word8 -> Int
timerPeriod register3 = 4 `max` (4 * (ratio register3 + 1) * (1 .<<. (shiftClock register3 + 1)))
