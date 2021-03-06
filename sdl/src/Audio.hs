module Audio
  ( Audio,
    start,
    pause,
    resume,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits ((.&.)))
import Data.Word (Word16, Word8)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (poke, pokeElemOff))
import qualified Machine.GBC.Audio as Audio
import qualified Machine.GBC.Emulator as Emulator
import Machine.GBC.Primitive.RingBuffer (RingBuffer)
import qualified Machine.GBC.Primitive.RingBuffer as RingBuffer
import Machine.GBC.Util ((.>>.))
import qualified SDL.Raw

newtype Audio = Audio SDL.Raw.AudioDeviceID

-- | Start the audio handler. The audio starts in the paused state, so it must
-- be resumed before any output will be heard.
start :: Emulator.State -> IO Audio
start emulatorState = do
  pAudioCallback <-
    SDL.Raw.mkAudioCallback . audioCallback . Audio.audioOut . Emulator.audioState $ emulatorState
  audioDevice <- alloca $ \desired -> alloca $ \actual -> do
    poke desired (desiredAudioSpec pAudioCallback)
    SDL.Raw.openAudioDevice nullPtr 0 desired actual 0
  pure (Audio audioDevice)

-- | Pause the audio output.
pause :: MonadIO m => Audio -> m ()
pause (Audio audioDevice) = SDL.Raw.pauseAudioDevice audioDevice (-1)

-- | Resume the audio output.
resume :: MonadIO m => Audio -> m ()
resume (Audio audioDevice) = SDL.Raw.pauseAudioDevice audioDevice 0

-- | Our preferred audio setup.
desiredAudioSpec :: SDL.Raw.AudioCallback -> SDL.Raw.AudioSpec
desiredAudioSpec callback =
  SDL.Raw.AudioSpec
    { SDL.Raw.audioSpecFreq = 44100,
      SDL.Raw.audioSpecFormat = SDL.Raw.SDL_AUDIO_U8,
      SDL.Raw.audioSpecChannels = 2,
      SDL.Raw.audioSpecSilence = 0,
      SDL.Raw.audioSpecSamples = 1024,
      SDL.Raw.audioSpecSize = 0,
      SDL.Raw.audioSpecCallback = callback,
      SDL.Raw.audioSpecUserdata = nullPtr
    }

-- | Callback to copy from the GBC output buffer to the SDL audio buffer.
audioCallback :: RingBuffer Word16 -> Ptr () -> Ptr Word8 -> CInt -> IO ()
audioCallback buffer _ stream len = do
  size <- RingBuffer.readableSize buffer
  if size == 0
    then pokeArray stream (replicate (fromIntegral len) 128)
    else void $
      RingBuffer.fold buffer (fromIntegral len `div` 2) 0 $ \i sample ->
        let left = fromIntegral (sample .&. 0x00FF)
            right = fromIntegral (sample .>>. 8)
         in do
              pokeElemOff stream i left
              pokeElemOff stream (i + 1) right
              pure (i + 2)
