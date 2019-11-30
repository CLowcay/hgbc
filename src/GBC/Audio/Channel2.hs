{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Channel2 where

import           Common
import           Control.Monad.Reader
import           Data.IORef
import           GBC.Audio.Common
import           GBC.Memory
import           GBC.Primitive
import           GBC.Registers

data Channel2 = Channel2 {
    output           :: IORef Int
  , enable           :: IORef Bool
  , frequencyCounter :: Counter
  , dutyCycle        :: StateCycle Int
  , volumeRef        :: IORef Int
  , volumeDeltaRef   :: IORef Int
  , envelopePeriodRef:: IORef Int
  , envelopeCounter  :: Counter
  , lengthCounter    :: Counter
}

makeChannel2 :: IO Channel2
makeChannel2 = do
  output            <- newIORef 0
  enable            <- newIORef False
  frequencyCounter  <- newCounter
  dutyCycle         <- newStateCycle dutyCycleStates
  volumeRef         <- newIORef 0
  volumeDeltaRef    <- newIORef 1
  envelopePeriodRef <- newIORef 0
  envelopeCounter   <- newCounter
  lengthCounter     <- newCounter
  pure Channel2 { .. }

instance Channel Channel2 where
  getOutput Channel2 {..} = liftIO $ readIORef output

  disable Channel2 {..} = liftIO $ do
    writeIORef output 0
    writeIORef enable False

  trigger Channel2 {..} = do
    liftIO $ writeIORef enable True
    l <- getCounter lengthCounter
    when (l == 0) $ reloadCounter lengthCounter 64
    reloadCounter frequencyCounter =<< getTimerPeriod NR23
    nr22 <- readByte NR22
    let envelopePeriod = getEnvelopePeriod nr22
    reloadCounter envelopeCounter envelopePeriod
    liftIO $ do
      writeIORef envelopePeriodRef envelopePeriod
      writeIORef volumeDeltaRef    (getVolumeDelta nr22)
      writeIORef volumeRef         (getVolume nr22)

  frameSequencerClock channel@Channel2 {..} FrameSequencerOutput {..} = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ do
      nr24 <- readByte NR24
      when (lengthClock && isFlagSet flagLength nr24)
        $  updateCounter lengthCounter 1
        $  0
        <$ disable channel

      envelopePeriod <- liftIO $ readIORef envelopePeriodRef
      when (envelopePeriod /= 0) $ updateCounter envelopeCounter 1 $ do
        liftIO $ do
          volume      <- readIORef volumeRef
          volumeDelta <- readIORef volumeDeltaRef
          writeIORef volumeRef (((volume + volumeDelta) `min` 15) `max` 0)
        pure envelopePeriod

  masterClock Channel2 {..} clockAdvance = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      void $ updateStateCycle dutyCycle 1 $ \i -> do
        dutyCycleNumber <- getDutyCycle <$> readByte NR21
        sample          <- liftIO $ readIORef volumeRef
        liftIO $ writeIORef output (if dutyCycleOutput dutyCycleNumber i then sample - 8 else 0)
      getTimerPeriod NR23

  writeX0 _ = pure ()

  writeX1 Channel2 {..} = do
    nr24 <- readByte NR24
    when (isFlagSet flagLength nr24) $ do
      nr21 <- readByte NR21
      reloadCounter lengthCounter (getLengthPeriod nr21)

  writeX2 _ = pure ()

  writeX3 Channel2 {..} = reloadCounter frequencyCounter =<< getTimerPeriod NR23
