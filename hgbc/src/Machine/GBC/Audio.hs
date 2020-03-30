{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Machine.GBC.Audio
  ( AudioState(..)
  , initAudioState
  , clockFrameSequencer
  , audioPorts
  , audioStep
  )
where

import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Bits
import           Data.Functor
import           Data.Word
import           Machine.GBC.Audio.Common
import           Machine.GBC.Audio.NoiseChannel
import           Machine.GBC.Audio.PulseChannel
import           Machine.GBC.Audio.WaveChannel
import           Machine.GBC.Primitive
import           Machine.GBC.Registers
import           Machine.GBC.Util

data AudioState = AudioState {
    audioOut       :: !(RingBuffer Word16)
  , sampler        :: !Counter
  , frameSequencer :: !(StateCycle FrameSequencerOutput)
  , port50         :: !(Port Word8)
  , port51         :: !(Port Word8)
  , port52         :: !(Port Word8)
  , portPCM12      :: !(Port Word8)
  , portPCM34      :: !(Port Word8)
  , channel1       :: !PulseChannel
  , channel2       :: !PulseChannel
  , channel3       :: !WaveChannel
  , channel4       :: !NoiseChannel
}

frameSequencerStates :: [(FrameSequencerOutput, Int)]
frameSequencerStates = (FrameSequencerOutput <$> (7 : [0 .. 6])) <&> (, 1)

initAudioState :: IO AudioState
initAudioState = mdo
  audioOut       <- newRingBuffer 12

  sampler        <- newCounter 0xFF
  frameSequencer <- newStateCycle frameSequencerStates

  channel1       <- newPulseChannel True port52 frameSequencer flagChannel1Enable
  channel2       <- newPulseChannel False port52 frameSequencer flagChannel2Enable
  channel3       <- newWaveChannel port52 frameSequencer
  channel4       <- newNoiseChannel port52 frameSequencer

  port50         <- newAudioPort port52 0xFF 0xFF alwaysUpdate
  port51         <- newAudioPort port52 0xFF 0xFF alwaysUpdate
  port52         <- newPortWithReadMask 0xFF 0x70 0x80 $ \register52 register52' -> do
    let masterPower  = isFlagSet flagMasterPower register52
    let masterPower' = isFlagSet flagMasterPower register52'
    if not masterPower' && masterPower
      then do
        powerOff channel1
        powerOff channel2
        powerOff channel3
        powerOff channel4
        directWritePort port50 0
        directWritePort port51 0
        resetStateCycle frameSequencer frameSequencerStates
        pure (register52' .&. 0xF0)
      else pure register52'

  portPCM12 <- newPort 0x00 0x00 neverUpdate
  portPCM34 <- newPort 0x00 0x00 neverUpdate

  pure AudioState { .. }

audioPorts :: AudioState -> [(Word16, Port Word8)]
audioPorts AudioState {..} =
  [(NR50, port50), (NR51, port51), (NR52, port52), (PCM12, portPCM12), (PCM34, portPCM34)]
    ++ channel1Ports
    ++ channel2Ports
    ++ channel3Ports
    ++ channel4Ports
 where
  channel1Ports = first ((+ NR10) . fromIntegral) <$> getPorts channel1
  channel2Ports = first ((+ NR20) . fromIntegral) <$> getPorts channel2
  channel3Ports = first ((+ NR30) . fromIntegral) <$> getPorts channel3
  channel4Ports = first ((+ NR40) . fromIntegral) <$> getPorts channel4

samplePeriod :: Int
samplePeriod = 94

mixOutputChannel :: (Int, Int, Int, Int) -> Word8 -> Word8
mixOutputChannel (v1, v2, v3, v4) channelFlags =
  let out1 = if channelFlags `testBit` 0 then v1 else 0
      out2 = if channelFlags `testBit` 1 then v2 else 0
      out3 = if channelFlags `testBit` 2 then v3 else 0
      out4 = if channelFlags `testBit` 3 then v4 else 0
  in  fromIntegral (((out1 + out2 + out3 + out4) * 4) + 128)

clockFrameSequencer :: AudioState -> IO ()
clockFrameSequencer AudioState {..} = do
  register52 <- directReadPort port52
  when (isFlagSet flagMasterPower register52)
    $ void
    $ updateStateCycle frameSequencer 1
    $ \state -> do
        frameSequencerClock channel1 state
        frameSequencerClock channel2 state
        frameSequencerClock channel3 state
        frameSequencerClock channel4 state

audioStep :: AudioState -> Int -> IO ()
audioStep AudioState {..} clockAdvance = do
  register52 <- directReadPort port52
  when (isFlagSet flagMasterPower register52) $ do
    masterClock channel1 clockAdvance
    masterClock channel2 clockAdvance
    masterClock channel3 clockAdvance
    masterClock channel4 clockAdvance

    updateCounter sampler clockAdvance $ do
      v1 <- getOutput channel1
      v2 <- getOutput channel2
      v3 <- getOutput channel3
      v4 <- getOutput channel4
      directWritePort portPCM12 (fromIntegral v1 .|. fromIntegral v2 .<<. 4)
      directWritePort portPCM34 (fromIntegral v3 .|. fromIntegral v4 .<<. 4)

      register50 <- directReadPort port50
      register51 <- directReadPort port51
      let volumeLeft  = 8 - (register50 .>>. 4 .&. 0x07)
      let volumeRight = 8 - (register50 .&. 0x07)
      let left        = register51 .>>. 4
      let right       = register51 .&. 0x0F

      let leftSample = mixOutputChannel (v1, v2, v3, v4) left `div` volumeLeft
      let rightSample = mixOutputChannel (v1, v2, v3, v4) right `div` volumeRight
      let stereo = fromIntegral leftSample .|. (fromIntegral rightSample .<<. 8)
      writeBuffer audioOut stereo
      pure samplePeriod