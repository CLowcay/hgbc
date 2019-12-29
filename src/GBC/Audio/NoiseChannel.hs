{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module GBC.Audio.NoiseChannel where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Audio.Envelope
import           GBC.Audio.Length
import           GBC.Primitive
import           GBC.Primitive.UnboxedRef

data NoiseChannel = NoiseChannel {
    enable           :: !(IORef Bool)
  , dacEnable        :: !(IORef Bool)
  , output           :: !(UnboxedRef Int)
  , port1            :: !(Port Word8)
  , port2            :: !(Port Word8)
  , port3            :: !(Port Word8)
  , port4            :: !(Port Word8)
  , port52           :: !(Port Word8)
  , lengthCounter    :: !Length
  , envelope         :: !Envelope
  , frequencyCounter :: !Counter
  , lfsr             :: !(LinearFeedbackShiftRegister Word16)
}

newNoiseChannel :: Port Word8 -> IO NoiseChannel
newNoiseChannel port52 = mdo
  enable    <- newIORef False
  dacEnable <- newIORef True
  output    <- newUnboxedRef 0

  port1     <- newPortWithReadMask 0xFF 0xFF 0x3F $ \_ register1 -> do
    register4 <- directReadPort port4
    when (isFlagSet flagLength register4) $ reloadLength lengthCounter register1
    pure register4

  port2 <- newPortWithReadMask 0xFF 0x00 0xFF $ \_ register2 -> do
    let isDacEnabled = register2 .&. 0xF8 /= 0
    unless isDacEnabled $ disableIO port52 output enable
    writeIORef dacEnable isDacEnabled
    pure register2

  port3 <- newPortWithReadMask 0xFF 0x00 0xFF $ \_ register3 -> do
    reloadCounter frequencyCounter (timerPeriod register3)
    pure register3

  port4 <- newPortWithReadMask 0xFF 0xBF 0xC0 $ \_ register4 -> do
    when (isFlagSet flagTrigger register4) $ do
      register2    <- directReadPort port2
      register3    <- directReadPort port3
      isDacEnabled <- readIORef dacEnable
      writeIORef enable isDacEnabled
      updateStatus port52 flagChannel4Enable isDacEnabled
      initLength lengthCounter
      initEnvelope envelope register2
      initLinearFeedbackShiftRegister 0xFFFF lfsr
      reloadCounter frequencyCounter (timerPeriod register3)
    pure register4

  lengthCounter    <- newLength 0x3F
  envelope         <- newEnvelope
  frequencyCounter <- newCounter
  lfsr             <- newLinearFeedbackShiftRegister
  pure NoiseChannel { .. }

disableIO :: Port Word8 -> UnboxedRef Int -> IORef Bool -> IO ()
disableIO port52 output enable = do
  writeUnboxedRef output 0
  writeIORef enable False
  updateStatus port52 flagChannel4Enable False

instance Channel NoiseChannel where
  getOutput NoiseChannel {..} = readUnboxedRef output
  disable NoiseChannel {..} = disableIO port52 output enable
  getStatus NoiseChannel {..} = readIORef enable
  getPorts NoiseChannel {..} = [(1, port1), (2, port2), (3, port3), (4, port4)]

  frameSequencerClock NoiseChannel {..} FrameSequencerOutput {..} = do
    register4 <- directReadPort port4
    when (lengthClock && isFlagSet flagLength register4)
      $ clockLength lengthCounter (disableIO port52 output enable)
    when envelopeClock $ clockEnvelope envelope

  masterClock NoiseChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      register3   <- directReadPort port3
      registerOut <- nextBit lfsr (widthMask register3)
      sample      <- envelopeVolume envelope
      writeUnboxedRef output ((if registerOut .&. 1 == 0 then sample else 0) - 8)
      pure (timerPeriod register3)

widthMask :: Word8 -> Word16
widthMask register3 = if register3 `testBit` 3 then 0x0040 else 0x4000

timerPeriod :: Word8 -> Int
timerPeriod register3 =
  let shiftClock = fromIntegral register3 .>>. 4
      ratio      = fromIntegral register3 .&. 0x07
  in  32 `max` (4 * (ratio + 1) * (1 .<<. (shiftClock + 1)))
