{-# LANGUAGE TupleSections #-}
{-# LANGUAGE  RecordWildCards #-}
module GBC.Audio.Channel3
  ( Channel3
  , makeChannel3
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.Functor
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Memory
import           Common
import           GBC.Primitive
import           GBC.Registers

data Channel3 = Channel3 {
    output           :: IORef Int
  , enable           :: IORef Bool
  , frequencyCounter :: Counter
  , sample           :: StateCycle Word16
  , lengthCounter    :: Counter
}

waveSamplerStates :: [(Word16, Int)]
waveSamplerStates = [0 .. 31] <&> (, 1)

makeChannel3 :: IO Channel3
makeChannel3 = do
  output           <- newIORef 0
  enable           <- newIORef False
  frequencyCounter <- newCounter
  sample           <- newStateCycle waveSamplerStates
  lengthCounter    <- newCounter
  pure Channel3 { .. }

flagChannel3Enable :: Word8
flagChannel3Enable = 0x80

instance Channel Channel3 where
  getOutput Channel3 {..} = liftIO $ readIORef output

  disable Channel3 {..} = liftIO $ do
    writeIORef output 0
    writeIORef enable False

  trigger Channel3 {..} = do
    liftIO $ writeIORef enable True
    l <- getCounter lengthCounter
    when (l == 0) $ reloadCounter lengthCounter 0xFF
    reloadCounter frequencyCounter . fromIntegral =<< getPTPeriod NR33
    resetStateCycle sample waveSamplerStates
    volume       <- getVolume
    masterEnable <- getMasterEnable
    when (volume == 0 || not masterEnable) $ liftIO $ writeIORef enable False

  frameSequencerClock channel@Channel3 {..} FrameSequencerOutput {..} = do
    nr34      <- readByte NR34
    isEnabled <- liftIO $ readIORef enable
    when (isEnabled && lengthClock && isFlagSet flagLength nr34)
      $  updateCounter lengthCounter 1
      $  0
      <$ disable channel

  masterClock Channel3 {..} clockAdvance = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      void $ updateStateCycle sample 1 $ \i -> do
        sampleByte <- readByte (0xFF30 + (i `unsafeShiftR` 1))
        volume <- getVolume
        let rawSampleValue =
              if i .&. 1 == 0 then sampleByte `unsafeShiftR` 4 else sampleByte .&. 0x0F
        let sampleValue =
              if volume == 0 then 0 else rawSampleValue `unsafeShiftR` (fromIntegral volume - 1)
        liftIO $ writeIORef output (fromIntegral sampleValue - 8)
      fromIntegral <$> getPTPeriod NR33

  writeX0 channel = do
    nr30 <- readByte NR30
    unless (isFlagSet flagChannel3Enable nr30) $ disable channel

  writeX1 Channel3 {..} = do
    nr34 <- readByte NR34
    when (isFlagSet flagLength nr34) $ do
      nr31 <- readByte NR31
      reloadCounter lengthCounter (fromIntegral $ negate nr31)

  writeX2 _ = pure ()

  writeX3 Channel3 {..} = reloadCounter frequencyCounter . fromIntegral =<< getPTPeriod NR33

getMasterEnable :: HasMemory env => ReaderT env IO Bool
getMasterEnable = do
  nr32 <- readByte NR30
  pure (nr32 .&. 0x80 /= 0)

getVolume :: HasMemory env => ReaderT env IO Word8
getVolume = do
  nr32 <- readByte NR32
  pure (3 .&. (nr32 `unsafeShiftR` 5))

