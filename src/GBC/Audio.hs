{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module GBC.Audio
  ( AudioState(..)
  , HasAudio(..)
  , initAudioState
  , enableAudioOut
  , disableAudioOut
  , audioStep
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.Functor
import           Data.IORef
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
import           GBC.CPU
import           GBC.Memory
import           GBC.Primitive
import           GBC.Registers
import qualified SDL.Raw

import           System.IO

desiredAudioSpec :: SDL.Raw.AudioCallback -> SDL.Raw.AudioSpec
desiredAudioSpec callback = SDL.Raw.AudioSpec { audioSpecFreq     = 44100
                                              , audioSpecFormat   = SDL.Raw.SDL_AUDIO_U8
                                              , audioSpecChannels = 2
                                              , audioSpecSilence  = 0
                                              , audioSpecSamples  = 512
                                              , audioSpecSize     = 0
                                              , audioSpecCallback = callback
                                              , audioSpecUserdata = nullPtr
                                              }

data AudioState = AudioState {
    audioDevice :: SDL.Raw.AudioDeviceID
  , audioOut :: RingBuffer Word16
  , outFile :: Handle
  , audioEnabled :: IORef Bool
  , sampler :: Counter
  , frameSequencer :: StateCycle Int
  , channel1 :: PulseChannel
  , channel2 :: PulseChannel
  , channel3 :: WaveChannel
  , channel4 :: NoiseChannel
}

frameSequencerStates :: [(Int, Int)]
frameSequencerStates = [0 .. 7] <&> (, 8192)

class HasMemory env => HasAudio env where
  forAudioState :: env -> AudioState

initAudioState :: IO AudioState
initAudioState = do
  audioOut       <- newRingBuffer 14

  outFile        <- openBinaryFile "out.pcm" WriteMode

  pAudioCallback <- SDL.Raw.mkAudioCallback (audioCallback audioOut)
  audioDevice    <- alloca $ \desired -> alloca $ \actual -> do
    poke desired (desiredAudioSpec pAudioCallback)
    SDL.Raw.openAudioDevice nullPtr 0 desired actual 0

  audioEnabled   <- newIORef True
  sampler        <- newCounter
  frameSequencer <- newStateCycle frameSequencerStates
  channel1       <- newPulseChannel NR10 True
  channel2       <- newPulseChannel NR20 False
  channel3       <- newWaveChannel
  channel4       <- newNoiseChannel
  pure AudioState { .. }

disableAudioOut :: HasAudio env => ReaderT env IO ()
disableAudioOut = do
  AudioState {..} <- asks forAudioState
  liftIO $ SDL.Raw.pauseAudioDevice audioDevice (-1)

enableAudioOut :: HasAudio env => ReaderT env IO ()
enableAudioOut = do
  AudioState {..} <- asks forAudioState
  liftIO $ SDL.Raw.pauseAudioDevice audioDevice 0

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

clearAllRegisters :: HasMemory env => ReaderT env IO ()
clearAllRegisters = do
  writeByte NR10 0x80
  writeByte NR11 0x00
  writeByte NR12 0x00
  writeByte NR13 0x00
  writeByte NR14 0x38
  writeByte NR20 0xFF
  writeByte NR21 0x00
  writeByte NR22 0x00
  writeByte NR23 0x00
  writeByte NR24 0x38
  writeByte NR30 0x7F
  writeByte NR31 0x00
  writeByte NR32 0x9F
  writeByte NR33 0x00
  writeByte NR34 0x38
  writeByte NR40 0xFF
  writeByte NR41 0xC0
  writeByte NR42 0x00
  writeByte NR43 0x00
  writeByte NR44 0x38

  writeByte NR50 0x00
  writeByte NR51 0x00
  writeByte NR52 0x8F

initAllRegisters :: HasMemory env => ReaderT env IO ()
initAllRegisters = do
  writeByte NR10 0x80
  writeByte NR11 0xBF
  writeByte NR12 0xF3
  writeByte NR14 0xBF
  writeByte NR21 0x3F
  writeByte NR22 0x00
  writeByte NR24 0xBF
  writeByte NR30 0x7F
  writeByte NR31 0xFF
  writeByte NR32 0x9F
  writeByte NR34 0xBF
  writeByte NR41 0xFF
  writeByte NR42 0x00
  writeByte NR43 0x00
  writeByte NR44 0xBF
  writeByte NR50 0x77
  writeByte NR51 0xF3

flagMasterPower :: Word8
flagMasterPower = 0x80

samplePeriod :: Int
samplePeriod = 94

mixOutputChannel :: HasAudio env => Word8 -> ReaderT env IO Word8
mixOutputChannel channelFlags = do
  AudioState {..} <- asks forAudioState
  out1            <- if channelFlags `testBit` 0 then getOutput channel1 else pure 0
  out2            <- if channelFlags `testBit` 1 then getOutput channel2 else pure 0
  out3            <- if channelFlags `testBit` 2 then getOutput channel3 else pure 0
  out4            <- if channelFlags `testBit` 3 then getOutput channel4 else pure 0
  pure (fromIntegral (((out1 + out2 + out3 + out4) * 4) + 128))

updateStatus :: HasAudio env => ReaderT env IO ()
updateStatus = do
  AudioState {..} <- asks forAudioState
  s1              <- getStatus channel1
  s2              <- getStatus channel2
  s3              <- getStatus channel3
  s4              <- getStatus channel4
  nr52            <- readByte NR52
  let status =
        (nr52 .&. 0xF0)
          .|. (if s1 then 1 else 0)
          .|. (if s2 then 2 else 0)
          .|. (if s3 then 4 else 0)
          .|. (if s4 then 8 else 0)
  writeByte NR52 status

audioStep :: HasAudio env => BusEvent -> ReaderT env IO ()
audioStep BusEvent { writeAddress, clockAdvance } = do
  AudioState {..} <- asks forAudioState
  for_ writeAddress $ \case
    NR52 -> do
      nr52 <- readByte NR52
      let masterPower = isFlagSet flagMasterPower nr52
      enabled <- liftIO $ readIORef audioEnabled
      when (masterPower && not enabled) $ do
        liftIO $ writeIORef audioEnabled masterPower
        initAllRegisters
      when (not masterPower && enabled) $ do
        disable channel3
        clearAllRegisters

    NR10 -> writeX0 channel1
    NR11 -> writeX1 channel1
    NR12 -> writeX2 channel1
    NR13 -> writeX3 channel1
    NR14 -> writeX4 channel1

    NR20 -> writeX0 channel2
    NR21 -> writeX1 channel2
    NR22 -> writeX2 channel2
    NR23 -> writeX3 channel2
    NR24 -> writeX4 channel2

    NR30 -> writeX0 channel3
    NR31 -> writeX1 channel3
    NR32 -> writeX2 channel3
    NR33 -> writeX3 channel3
    NR34 -> writeX4 channel3

    NR40 -> writeX0 channel4
    NR41 -> writeX1 channel4
    NR42 -> writeX2 channel4
    NR43 -> writeX3 channel4
    NR44 -> writeX4 channel4
    _    -> pure ()

  updateStatus

  enabled <- liftIO $ readIORef audioEnabled
  when enabled $ do
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
            updateStatus
    masterClock channel1 clockAdvance
    masterClock channel2 clockAdvance
    masterClock channel3 clockAdvance
    masterClock channel4 clockAdvance

    updateCounter sampler clockAdvance $ do
      nr50 <- readByte NR50
      nr51 <- readByte NR51
      let volumeLeft  = 8 - (nr50 .&. 0x07)
      let volumeRight = 8 - (nr50 `unsafeShiftR` 4 .&. 0x07)
      let left        = nr51 .&. 0x0F
      let right       = nr51 `unsafeShiftR` 4
      leftSample  <- (`div` volumeLeft) <$> mixOutputChannel left
      rightSample <- (`div` volumeRight) <$> mixOutputChannel right
      let stereo = fromIntegral leftSample .|. (fromIntegral rightSample `unsafeShiftL` 8)
      liftIO $ writeBuffer audioOut stereo
      liftIO $ alloca $ \pBuf -> poke pBuf stereo >> hPutBuf outFile pBuf 2
      pure samplePeriod
