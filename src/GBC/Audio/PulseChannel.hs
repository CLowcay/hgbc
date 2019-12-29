{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module GBC.Audio.PulseChannel
  ( PulseChannel
  , newPulseChannel
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Functor
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Audio.Envelope
import           GBC.Audio.Length
import           GBC.Audio.Sweep
import           GBC.Primitive
import           GBC.Primitive.UnboxedRef

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
  , dutyCycle          :: !(StateCycle Int)
  , envelope           :: !Envelope
  , lengthCounter      :: !Length
}

newPulseChannel :: Bool -> Port Word8 -> Word8 -> IO PulseChannel
newPulseChannel hasSweepUnit port52 channelEnabledFlag = mdo
  output    <- newUnboxedRef 0
  enable    <- newIORef False
  dacEnable <- newIORef True

  let port0ReadMask  = if hasSweepUnit then 0x80 else 0xFF
  let port0WriteMask = if hasSweepUnit then 0x7F else 0x00
  port0 <- newPortWithReadMask 0xFF port0ReadMask port0WriteMask $ \_ register0 -> do
    when hasSweepUnit $ do
      hasNegated <- hasPerformedSweepCalculationInNegateMode sweepUnit
      when (hasNegated && not (isFlagSet flagNegate register0))
        $ disableIO port52 channelEnabledFlag output enable
    pure register0

  port1 <- newPortWithReadMask 0xFF 0x3F 0xFF $ \_ register1 -> do
    register4 <- directReadPort port4
    when (isFlagSet flagLength register4) $ reloadLength lengthCounter register1
    pure register1

  port2 <- newPort 0xFF 0xFF $ \_ register2 -> do
    let isDacEnabled = register2 .&. 0xF8 /= 0
    unless isDacEnabled $ disableIO port52 channelEnabledFlag output enable
    writeIORef dacEnable isDacEnabled
    pure register2

  port3 <- newPortWithReadMask 0xFF 0xFF 0xFF $ \_ register3 -> do
    register4 <- directReadPort port4
    let frequency = getFrequency register3 register4
    reloadCounter frequencyCounter (getTimerPeriod frequency)
    pure register3

  port4 <- newPortWithReadMask 0xFF 0xBF 0xC7 $ \_ register4 -> do
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
      initLength lengthCounter
      initEnvelope envelope register2
      reloadCounter frequencyCounter (getTimerPeriod frequency)
    pure register4

  sweepUnit        <- newSweep port3 port4
  frequencyCounter <- newCounter
  dutyCycle        <- newStateCycle dutyCycleStates
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

  frameSequencerClock channel@PulseChannel {..} FrameSequencerOutput {..} = do
    register4 <- directReadPort port4
    when (lengthClock && isFlagSet flagLength register4)
      $ clockLength lengthCounter (disable channel)
    when envelopeClock $ clockEnvelope envelope
    when (sweepClock && hasSweepUnit) $ do
      register0 <- directReadPort port0
      clockSweep sweepUnit register0 (disable channel)

  masterClock PulseChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      void $ updateStateCycle dutyCycle 1 $ \i -> do
        dutyCycleNumber <- getDutyCycle <$> directReadPort port1
        sample          <- envelopeVolume envelope
        writeUnboxedRef output ((if dutyCycleOutput dutyCycleNumber i then sample else 0) - 8)
      register3 <- directReadPort port3
      register4 <- directReadPort port4
      pure (getTimerPeriod (getFrequency register3 register4))

getDutyCycle :: Word8 -> Word8
getDutyCycle register = register .>>. 6

dutyCycleStates :: [(Int, Int)]
dutyCycleStates = [1, 2, 3, 4, 5, 6, 7, 0] <&> (, 1)

dutyCycleOutput :: Word8 -> Int -> Bool
dutyCycleOutput 0 i = i == 0
dutyCycleOutput 1 i = i <= 1
dutyCycleOutput 2 i = i <= 1 || i >= 6
dutyCycleOutput 3 i = i > 1
dutyCycleOutput x _ = error ("Invalid duty cycle " <> show x)

getTimerPeriod :: Int -> Int
getTimerPeriod f = (4 * (2048 - f)) - 1
