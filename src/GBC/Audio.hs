{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module GBC.Audio
  ( AudioState(..)
  , initAudioState
  , audioPorts
  , enableAudioOut
  , disableAudioOut
  , audioStep
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Bits
import           Data.Foldable
import           Data.Functor
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Audio.Common
import           GBC.Audio.NoiseChannel
import           GBC.Audio.PulseChannel
import           GBC.Audio.WaveChannel
import           GBC.Primitive
import           GBC.Registers
import qualified SDL.Raw

desiredAudioSpec :: SDL.Raw.AudioCallback -> SDL.Raw.AudioSpec
desiredAudioSpec callback = SDL.Raw.AudioSpec { audioSpecFreq     = 44100
                                              , audioSpecFormat   = SDL.Raw.SDL_AUDIO_U8
                                              , audioSpecChannels = 2
                                              , audioSpecSilence  = 0
                                              , audioSpecSamples  = 1024
                                              , audioSpecSize     = 0
                                              , audioSpecCallback = callback
                                              , audioSpecUserdata = nullPtr
                                              }

data AudioState = AudioState {
    audioDevice    :: !SDL.Raw.AudioDeviceID
  , audioOut       :: !(RingBuffer Word16)
  , sampler        :: !Counter
  , frameSequencer :: !(StateCycle Int)
  , port50         :: !(Port Word8)
  , port51         :: !(Port Word8)
  , port52         :: !(Port Word8)
  , channel1       :: !PulseChannel
  , channel2       :: !PulseChannel
  , channel3       :: !WaveChannel
  , channel4       :: !NoiseChannel
}

frameSequencerStates :: [(Int, Int)]
frameSequencerStates = [0 .. 7] <&> (, 8192)

initAudioState :: IO AudioState
initAudioState = mdo
  audioOut       <- newRingBuffer 12

  pAudioCallback <- SDL.Raw.mkAudioCallback (audioCallback audioOut)
  audioDevice    <- alloca $ \desired -> alloca $ \actual -> do
    poke desired (desiredAudioSpec pAudioCallback)
    SDL.Raw.openAudioDevice nullPtr 0 desired actual 0

  sampler        <- newCounter
  frameSequencer <- newStateCycle frameSequencerStates

  port50         <- newPort 0xFF 0xFF alwaysUpdate
  port51         <- newPort 0xFF 0xFF alwaysUpdate
  port52         <- newPortWithReadMask 0xFF 0x70 0x80 $ \register52 register52' -> do
    let masterPower  = isFlagSet flagMasterPower register52
    let masterPower' = isFlagSet flagMasterPower register52'
    when (not masterPower' && masterPower) $ do
      disable channel1
      disable channel2
      disable channel3
      disable channel4
      let write0 = flip directWritePort 0
      traverse_ write0 $ snd <$> getPorts channel1
      traverse_ write0 $ snd <$> getPorts channel2
      traverse_ write0 $ snd <$> filter ((<= 4) . fst) (getPorts channel3)
      traverse_ write0 $ snd <$> getPorts channel4
      directWritePort port50 0
      directWritePort port51 0
    pure register52'

  channel1 <- newPulseChannel True port52 flagChannel1Enable
  channel2 <- newPulseChannel False port52 flagChannel2Enable
  channel3 <- newWaveChannel port52
  channel4 <- newNoiseChannel port52
  pure AudioState { .. }

audioPorts :: AudioState -> [(Word16, Port Word8)]
audioPorts AudioState {..} =
  [(NR50, port50), (NR51, port51), (NR52, port52)]
    ++ channel1Ports
    ++ channel2Ports
    ++ channel3Ports
    ++ channel4Ports
 where
  channel1Ports = first ((+ NR10) . fromIntegral) <$> getPorts channel1
  channel2Ports = first ((+ NR20) . fromIntegral) <$> getPorts channel2
  channel3Ports = first ((+ NR30) . fromIntegral) <$> getPorts channel3
  channel4Ports = first ((+ NR40) . fromIntegral) <$> getPorts channel4

disableAudioOut :: AudioState -> IO ()
disableAudioOut AudioState {..} = SDL.Raw.pauseAudioDevice audioDevice (-1)

enableAudioOut :: AudioState -> IO ()
enableAudioOut AudioState {..} = SDL.Raw.pauseAudioDevice audioDevice 0

audioCallback :: RingBuffer Word16 -> Ptr () -> Ptr Word8 -> CInt -> IO ()
audioCallback buffer _ stream len = do
  size <- readableSize buffer
  if size == 0
    then pokeArray stream (replicate (fromIntegral len) 128)
    else void $ foldBuffer buffer (fromIntegral len `div` 2) 0 $ \i sample ->
      let left  = fromIntegral (sample .&. 0x00FF)
          right = fromIntegral (sample `unsafeShiftR` 8)
      in  do
            pokeElemOff stream i       left
            pokeElemOff stream (i + 1) right
            pure (i + 2)

flagMasterPower :: Word8
flagMasterPower = 0x80

samplePeriod :: Int
samplePeriod = 94

mixOutputChannel :: AudioState -> Word8 -> IO Word8
mixOutputChannel AudioState {..} channelFlags = do
  out1 <- if channelFlags `testBit` 0 then getOutput channel1 else pure 0
  out2 <- if channelFlags `testBit` 1 then getOutput channel2 else pure 0
  out3 <- if channelFlags `testBit` 2 then getOutput channel3 else pure 0
  out4 <- if channelFlags `testBit` 3 then getOutput channel4 else pure 0
  pure (fromIntegral (((out1 + out2 + out3 + out4) * 4) + 128))

audioStep :: AudioState -> Int -> IO ()
audioStep audioState@AudioState {..} clockAdvance = do
  register52 <- readPort port52
  when (isFlagSet flagMasterPower register52) $ do
    void $ updateStateCycle frameSequencer clockAdvance $ \state ->
      let frameSequencerOutput = case state of
            0 -> FrameSequencerOutput True False False
            1 -> FrameSequencerOutput False False False
            2 -> FrameSequencerOutput True False True
            3 -> FrameSequencerOutput False False False
            4 -> FrameSequencerOutput True False False
            5 -> FrameSequencerOutput False False False
            6 -> FrameSequencerOutput True False True
            7 -> FrameSequencerOutput False True False
            x -> error ("Invalid frame sequencer state " <> show x)
      in  do
            frameSequencerClock channel1 frameSequencerOutput
            frameSequencerClock channel2 frameSequencerOutput
            frameSequencerClock channel3 frameSequencerOutput
            frameSequencerClock channel4 frameSequencerOutput
    masterClock channel1 clockAdvance
    masterClock channel2 clockAdvance
    masterClock channel3 clockAdvance
    masterClock channel4 clockAdvance

    updateCounter sampler clockAdvance $ do
      register50 <- readPort port50
      register51 <- readPort port51
      let volumeLeft  = 8 - (register50 `unsafeShiftR` 4 .&. 0x07)
      let volumeRight = 8 - (register50 .&. 0x07)
      let left        = register51 `unsafeShiftR` 4
      let right       = register51 .&. 0x0F
      leftSample  <- (`div` volumeLeft) <$> mixOutputChannel audioState left
      rightSample <- (`div` volumeRight) <$> mixOutputChannel audioState right
      let stereo = fromIntegral leftSample .|. (fromIntegral rightSample `unsafeShiftL` 8)
      writeBuffer audioOut stereo
      pure samplePeriod
