{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.WaveChannel
  ( WaveChannel
  , newWaveChannel
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Functor
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Audio.Length
import           GBC.Memory
import           GBC.Primitive
import           GBC.Registers

data WaveChannel = WaveChannel {
    output           :: IORef Int
  , enable           :: IORef Bool
  , frequencyCounter :: Counter
  , sample           :: StateCycle Word16
  , lengthCounter    :: Length
}

waveSamplerStates :: [(Word16, Int)]
waveSamplerStates = [0 .. 31] <&> (, 1)

newWaveChannel :: IO WaveChannel
newWaveChannel = do
  output           <- newIORef 0
  enable           <- newIORef False
  frequencyCounter <- newCounter
  sample           <- newStateCycle waveSamplerStates
  lengthCounter    <- newLength 0xFF
  pure WaveChannel { .. }

flagChannel3Enable :: Word8
flagChannel3Enable = 0x80

disableIO :: WaveChannel -> IO ()
disableIO WaveChannel {..} = do
  writeIORef output 0
  writeIORef enable False

instance Channel WaveChannel where
  getOutput WaveChannel {..} = liftIO $ readIORef output

  disable = liftIO . disableIO

  getStatus WaveChannel {..} = liftIO $ readIORef enable

  trigger WaveChannel {..} = do
    liftIO $ writeIORef enable True
    liftIO $ initLength lengthCounter
    reloadCounter frequencyCounter =<< getTimerPeriod
    resetStateCycle sample waveSamplerStates
    volume       <- getVolume
    masterEnable <- getMasterEnable
    when (volume == 0 || not masterEnable) $ liftIO $ writeIORef enable False

  frameSequencerClock channel@WaveChannel {..} FrameSequencerOutput {..} = do
    register4 <- readByte NR34
    isEnabled <- liftIO $ readIORef enable
    liftIO $ when (isEnabled && lengthClock && isFlagSet flagLength register4) $ clockLength
      lengthCounter
      (disableIO channel)

  masterClock WaveChannel {..} clockAdvance = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      void $ updateStateCycle sample 1 $ \i -> do
        sampleByte <- readByte (0xFF30 + (i `unsafeShiftR` 1))
        volume     <- getVolume
        let rawSampleValue =
              if i .&. 1 == 0 then sampleByte `unsafeShiftR` 4 else sampleByte .&. 0x0F
        let sampleValue =
              if volume == 0 then 0 else rawSampleValue `unsafeShiftR` (fromIntegral volume - 1)
        liftIO $ writeIORef output (fromIntegral sampleValue - 8)
      getTimerPeriod

  writeX0 channel = do
    register0 <- readByte NR30
    unless (isFlagSet flagChannel3Enable register0) $ disable channel

  writeX1 WaveChannel {..} = do
    register4 <- readByte NR34
    when (isFlagSet flagLength register4) $ do
      register1 <- readByte NR31
      liftIO $ reloadLength lengthCounter register1

  writeX2 _ = pure ()

  writeX3 WaveChannel {..} = reloadCounter frequencyCounter =<< getTimerPeriod

  writeX4 channel@WaveChannel {..} = do
    register4 <- readByte NR34
    when (isFlagSet flagTrigger register4) $ trigger channel

getMasterEnable :: HasMemory env => ReaderT env IO Bool
getMasterEnable = do
  register2 <- readByte NR30
  pure (register2 .&. 0x80 /= 0)

getVolume :: HasMemory env => ReaderT env IO Word8
getVolume = do
  register2 <- readByte NR32
  pure (3 .&. (register2 `unsafeShiftR` 5))

getTimerPeriod :: HasMemory env => ReaderT env IO Int
getTimerPeriod = do
  register3 <- readByte NR33
  register4 <- readByte NR34
  let f = ((fromIntegral register4 .&. 0x07) `unsafeShiftL` 8) .|. fromIntegral register3
  pure ((2 * (2048 - f)) - 1)
