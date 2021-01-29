{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Machine.GBC.Audio
  ( State (..),
    init,
    clockFrameSequencer,
    ports,
    step,
  )
where

import Control.Monad.Reader (void, when)
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (Bits (testBit, (.&.), (.|.)))
import Data.Functor ((<&>))
import Data.Word (Word16, Word8)
import Machine.GBC.Audio.Common (Channel (..), FrameSequencerOutput (..), flagChannel1Enable, flagChannel2Enable, flagMasterPower, newAudioPort)
import Machine.GBC.Audio.NoiseChannel (NoiseChannel, newNoiseChannel)
import Machine.GBC.Audio.PulseChannel (PulseChannel, newPulseChannel)
import Machine.GBC.Audio.WaveChannel (WaveChannel, newWaveChannel)
import Machine.GBC.Primitive.Counter (Counter)
import qualified Machine.GBC.Primitive.Counter as Counter
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port
import Machine.GBC.Primitive.RingBuffer (RingBuffer)
import qualified Machine.GBC.Primitive.RingBuffer as RingBuffer
import Machine.GBC.Primitive.StateCycle (StateCycle)
import qualified Machine.GBC.Primitive.StateCycle as StateCycle
import qualified Machine.GBC.Registers as R
import Machine.GBC.Util (isFlagSet, (.<<.), (.>>.))
import Prelude hiding (init)

data State = State
  { audioOut :: !(RingBuffer Word16),
    sampler :: !Counter,
    frameSequencer :: !(StateCycle FrameSequencerOutput),
    port50 :: !Port,
    port51 :: !Port,
    port52 :: !Port,
    portPCM12 :: !Port,
    portPCM34 :: !Port,
    channel1 :: !PulseChannel,
    channel2 :: !PulseChannel,
    channel3 :: !WaveChannel,
    channel4 :: !NoiseChannel
  }

frameSequencerStates :: [(FrameSequencerOutput, Int)]
frameSequencerStates = (FrameSequencerOutput <$> (7 : [0 .. 6])) <&> (,1)

init :: IO State
init = mdo
  audioOut <- RingBuffer.new 12

  sampler <- Counter.new 0xFF
  frameSequencer <- StateCycle.new frameSequencerStates

  channel1 <- newPulseChannel True port52 frameSequencer flagChannel1Enable
  channel2 <- newPulseChannel False port52 frameSequencer flagChannel2Enable
  channel3 <- newWaveChannel port52 frameSequencer
  channel4 <- newNoiseChannel port52 frameSequencer

  port50 <- newAudioPort port52 0xFF 0xFF Port.alwaysUpdate
  port51 <- newAudioPort port52 0xFF 0xFF Port.alwaysUpdate
  port52 <- Port.newWithReadMask 0xFF 0x70 0x80 $ \register52 register52' -> do
    let masterPower = isFlagSet flagMasterPower register52
    let masterPower' = isFlagSet flagMasterPower register52'
    if not masterPower' && masterPower
      then do
        powerOff channel1
        powerOff channel2
        powerOff channel3
        powerOff channel4
        Port.writeDirect port50 0
        Port.writeDirect port51 0
        StateCycle.reset frameSequencer frameSequencerStates
        pure (register52' .&. 0xF0)
      else pure register52'

  portPCM12 <- Port.new 0x00 0x00 Port.neverUpdate
  portPCM34 <- Port.new 0x00 0x00 Port.neverUpdate

  pure State {..}

ports :: State -> [(Word16, Port)]
ports State {..} =
  [(R.NR50, port50), (R.NR51, port51), (R.NR52, port52), (R.PCM12, portPCM12), (R.PCM34, portPCM34)]
    ++ channel1Ports
    ++ channel2Ports
    ++ channel3Ports
    ++ channel4Ports
  where
    channel1Ports = first ((+ R.NR10) . fromIntegral) <$> getPorts channel1
    channel2Ports = first ((+ R.NR20) . fromIntegral) <$> getPorts channel2
    channel3Ports = first ((+ R.NR30) . fromIntegral) <$> getPorts channel3
    channel4Ports = first ((+ R.NR40) . fromIntegral) <$> getPorts channel4

samplePeriod :: Int
samplePeriod = 94

mixOutputChannel :: (Int, Int, Int, Int) -> Word8 -> Word8
mixOutputChannel (v1, v2, v3, v4) channelFlags =
  let out1 = if channelFlags `testBit` 0 then v1 else 8
      out2 = if channelFlags `testBit` 1 then v2 else 8
      out3 = if channelFlags `testBit` 2 then v3 else 8
      out4 = if channelFlags `testBit` 3 then v4 else 8
   in fromIntegral (((out1 + out2 + out3 + out4 - 32) * 4) + 128)

clockFrameSequencer :: State -> IO ()
clockFrameSequencer State {..} = do
  register52 <- Port.readDirect port52
  when (isFlagSet flagMasterPower register52) $
    void $
      StateCycle.update frameSequencer 1 $
        \state -> do
          frameSequencerClock channel1 state
          frameSequencerClock channel2 state
          frameSequencerClock channel3 state
          frameSequencerClock channel4 state

step :: State -> Int -> IO ()
step State {..} clockAdvance = do
  register52 <- Port.readDirect port52
  when (isFlagSet flagMasterPower register52) $ do
    masterClock channel1 clockAdvance
    masterClock channel2 clockAdvance
    masterClock channel3 clockAdvance
    masterClock channel4 clockAdvance

    Counter.update sampler clockAdvance $ do
      v1 <- getOutput channel1
      v2 <- getOutput channel2
      v3 <- getOutput channel3
      v4 <- getOutput channel4
      Port.writeDirect portPCM12 (fromIntegral v1 .|. fromIntegral (v2 .<<. 4))
      Port.writeDirect portPCM34 (fromIntegral v3 .|. fromIntegral (v4 .<<. 4))

      register50 <- Port.readDirect port50
      register51 <- Port.readDirect port51
      let volumeLeft = 8 - (register50 .>>. 4 .&. 0x07)
      let volumeRight = 8 - (register50 .&. 0x07)
      let left = register51 .>>. 4
      let right = register51 .&. 0x0F

      let leftSample = mixOutputChannel (v1, v2, v3, v4) left `div` volumeLeft
      let rightSample = mixOutputChannel (v1, v2, v3, v4) right `div` volumeRight
      let stereo = fromIntegral leftSample .|. (fromIntegral rightSample .<<. 8)
      RingBuffer.write audioOut stereo
      pure samplePeriod
