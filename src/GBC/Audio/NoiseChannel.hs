{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.NoiseChannel where

import           Common
import           Control.Monad.Reader
import           Data.IORef
import           Data.Word
import           Data.Bits
import           GBC.Audio.Common
import           GBC.Audio.Envelope
import           GBC.Audio.Length
import           GBC.Memory
import           GBC.Primitive
import           GBC.Registers

data NoiseChannel = NoiseChannel {
    enable           :: IORef Bool
  , dacEnable        :: IORef Bool
  , output           :: IORef Int
  , lengthCounter    :: Length
  , envelope         :: Envelope
  , frequencyCounter :: Counter
  , lfsr             :: LinearFeedbackShiftRegister Word16
}

newNoiseChannel :: IO NoiseChannel
newNoiseChannel = do
  enable           <- newIORef False
  dacEnable        <- newIORef True
  output           <- newIORef 0
  lengthCounter    <- newLength 0x3F
  envelope         <- newEnvelope
  frequencyCounter <- newCounter
  lfsr             <- newLinearFeedbackShiftRegister
  pure NoiseChannel { .. }

disableIO :: NoiseChannel -> IO ()
disableIO NoiseChannel {..} = do
  writeIORef output 0
  writeIORef enable False

instance Channel NoiseChannel where
  getOutput NoiseChannel {..} = liftIO $ readIORef output

  disable = liftIO . disableIO

  getStatus NoiseChannel {..} = liftIO $ readIORef enable

  trigger NoiseChannel {..} = do
    register2 <- readByte NR42
    liftIO $ do
      isDacEnabled <- readIORef dacEnable
      writeIORef enable isDacEnabled
      initLength lengthCounter
      initEnvelope envelope register2
      initLinearFeedbackShiftRegister 0xFFFF lfsr
    reloadCounter frequencyCounter =<< getTimerPeriod

  frameSequencerClock channel@NoiseChannel {..} FrameSequencerOutput {..} = do
    register4 <- readByte NR44
    liftIO $ do
      when (lengthClock && isFlagSet flagLength register4)
        $ clockLength lengthCounter (disableIO channel)
      when envelopeClock $ clockEnvelope envelope

  masterClock NoiseChannel {..} clockAdvance = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      width <- getWidth
      liftIO $ do
        outBit <- nextBit lfsr width
        sample <- envelopeVolume envelope
        writeIORef output ((if outBit then sample else 0) - 8)
      getTimerPeriod

  writeX0 _ = pure ()

  writeX1 NoiseChannel {..} = do
    register4 <- readByte NR44
    when (isFlagSet flagLength register4) $ do
      register1 <- readByte NR41
      liftIO $ reloadLength lengthCounter register1

  writeX2 channel@NoiseChannel {..} = do
    register2 <- readByte NR42
    let isDacEnabled = register2 .&. 0xF8 /= 0
    unless isDacEnabled $ disable channel
    liftIO $ writeIORef dacEnable isDacEnabled

  writeX3 NoiseChannel {..} = reloadCounter frequencyCounter =<< getTimerPeriod

  writeX4 channel@NoiseChannel {..} = do
    register4 <- readByte NR44
    when (isFlagSet flagTrigger register4) $ trigger channel

getWidth :: HasMemory env => ReaderT env IO Int
getWidth = do
  register3 <- readByte NR43
  pure (if register3 `testBit` 3 then 6 else 14)

getTimerPeriod :: HasMemory env => ReaderT env IO Int
getTimerPeriod = do
  register3 <- readByte NR43
  pure (timerPeriod (fromIntegral register3 `unsafeShiftR` 4) (fromIntegral register3 .&. 0x07))

timerPeriod :: Int -> Int -> Int
timerPeriod shiftClock ratio = 4 * (ratio + 1) * (1 `unsafeShiftL` (shiftClock + 1))
