{-# LANGUAGE RecordWildCards #-}
module GBC.Audio
  ( AudioState(..)
  , HasAudio(..)
  , initAudioState
  , enableAudioOut
  , disableAudioOut
  )
where

import           Control.Monad.Reader
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Memory
import qualified SDL.Raw

data AudioState = AudioState {
  audioDevice :: SDL.Raw.AudioDeviceID
}

class HasMemory env => HasAudio env where
  forAudioState :: env -> AudioState

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

initAudioState :: IO AudioState
initAudioState = do
  pAudioCallback <- SDL.Raw.mkAudioCallback audioCallback
  audioDevice    <- alloca $ \desired -> alloca $ \actual -> do
    poke desired (desiredAudioSpec pAudioCallback)
    SDL.Raw.openAudioDevice nullPtr 0 desired actual 0

  pure AudioState { .. }

disableAudioOut :: HasAudio env => ReaderT env IO ()
disableAudioOut = do
  AudioState {..} <- asks forAudioState
  liftIO $ SDL.Raw.pauseAudioDevice audioDevice (-1)

enableAudioOut :: HasAudio env => ReaderT env IO ()
enableAudioOut = do
  AudioState {..} <- asks forAudioState
  liftIO $ SDL.Raw.pauseAudioDevice audioDevice 0

audioCallback :: Ptr () -> Ptr Word8 -> CInt -> IO ()
audioCallback _ stream len =
  let samples = round . (\x -> 255 * sin (x / 50 :: Float)) <$> [0 .. 49]
  in  pokeArray stream . take (fromIntegral len) $ cycle samples
