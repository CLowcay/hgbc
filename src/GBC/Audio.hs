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
import           GBC.Audio.Channel3
import           GBC.Audio.Common
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
  , channel3 :: Channel3
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
  channel3       <- makeChannel3
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
    then pokeArray stream [0 .. (fromIntegral len - 1)]
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
  out             <- getOutput channel3
  pure (fromIntegral ((out * 4) + 128))

  --(channelFlags `testBit`) <$> [0..3]



  --samples         <- liftIO $ traverse
  --  (\(i, channel) -> if channelFlags `testBit` i then readIORef channel else pure 0)
  --  ([0 .. 3] `zip` [channel1, channel2, channel3, channel4])
  --pure (fromIntegral ((17 * sum samples) `div` 4))

audioStep :: HasAudio env => BusEvent -> ReaderT env IO ()
audioStep BusEvent { writeAddress, clockAdvance } = do
  AudioState {..} <- asks forAudioState
  for_ writeAddress $ \case
    NR52 -> do
      nr52 <- readByte NR52
      let masterPower = isFlagSet flagMasterPower nr52
      liftIO $ writeIORef audioEnabled masterPower
      if masterPower
        then initAllRegisters
        else do
          disable channel3
          clearAllRegisters
    NR30 -> writeX0 channel3
    NR31 -> writeX1 channel3
    NR32 -> writeX2 channel3
    NR33 -> writeX3 channel3
    NR34 -> do
      nr34 <- readByte NR34
      when (isFlagSet flagTrigger nr34) $ trigger channel3

    _ -> pure ()

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
      in  frameSequencerClock channel3 frameSequencerOutput
    masterClock channel3 clockAdvance

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
