{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.PulseChannel
  ( PulseChannel
  , newPulseChannel
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Functor
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Audio.Envelope
import           GBC.Audio.Length
import           GBC.Memory
import           GBC.Primitive

data PulseChannel = PulseChannel {
    output           :: IORef Int
  , enable           :: IORef Bool
  , baseRegister     :: Word16
  , frequencyCounter :: Counter
  , dutyCycle        :: StateCycle Int
  , envelope         :: Envelope
  , lengthCounter    :: Length
}

newPulseChannel :: Word16 -> IO PulseChannel
newPulseChannel baseRegister = do
  output           <- newIORef 0
  enable           <- newIORef False
  frequencyCounter <- newCounter
  dutyCycle        <- newStateCycle dutyCycleStates
  envelope         <- newEnvelope
  lengthCounter    <- newLength 0x3F
  pure PulseChannel { .. }

disableIO :: PulseChannel -> IO ()
disableIO PulseChannel {..} = do
  writeIORef output 0
  writeIORef enable False

instance Channel PulseChannel where
  getOutput PulseChannel {..} = liftIO $ readIORef output

  disable = liftIO . disableIO

  getStatus PulseChannel {..} = liftIO $ readIORef enable

  trigger PulseChannel {..} = do
    register2 <- readByte (baseRegister + 2)
    liftIO $ do
      writeIORef enable True
      initLength lengthCounter
      initEnvelope envelope register2
    reloadCounter frequencyCounter =<< getTimerPeriod baseRegister

  frameSequencerClock channel@PulseChannel {..} FrameSequencerOutput {..} = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ do
      register4 <- readByte (baseRegister + 4)
      liftIO $ do
        when (lengthClock && isFlagSet flagLength register4)
          $ clockLength lengthCounter (disableIO channel)
        when envelopeClock $ clockEnvelope envelope

  masterClock PulseChannel {..} clockAdvance = do
    isEnabled <- liftIO $ readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      void $ updateStateCycle dutyCycle 1 $ \i -> do
        dutyCycleNumber <- getDutyCycle <$> readByte (baseRegister + 1)
        sample          <- liftIO $ envelopeVolume envelope
        liftIO $ writeIORef output ((if dutyCycleOutput dutyCycleNumber i then sample else 0) - 8)
      getTimerPeriod baseRegister

  writeX0 _ = pure ()

  writeX1 PulseChannel {..} = do
    register4 <- readByte (baseRegister + 4)
    when (isFlagSet flagLength register4) $ do
      register1 <- readByte (baseRegister + 1)
      liftIO $ reloadLength lengthCounter register1

  writeX2 _ = pure ()

  writeX3 PulseChannel {..} = reloadCounter frequencyCounter =<< getTimerPeriod baseRegister

  writeX4 channel@PulseChannel {..} = do
    register4 <- readByte (baseRegister + 4)
    when (isFlagSet flagTrigger register4) $ trigger channel

getDutyCycle :: Word8 -> Word8
getDutyCycle register = register `unsafeShiftR` 6

dutyCycleStates :: [(Int, Int)]
dutyCycleStates = [1, 2, 3, 4, 5, 6, 7, 0] <&> (, 1)

dutyCycleOutput :: Word8 -> Int -> Bool
dutyCycleOutput 0 i = i == 0
dutyCycleOutput 1 i = i <= 1
dutyCycleOutput 2 i = i <= 4
dutyCycleOutput 3 i = i > 1
dutyCycleOutput x _ = error ("Invalid duty cycle " <> show x)

getTimerPeriod :: HasMemory env => Word16 -> ReaderT env IO Int
getTimerPeriod base = do
  lsb <- readByte (base + 3)
  msb <- readByte (base + 4)
  let f = ((fromIntegral msb .&. 0x07) `unsafeShiftL` 8) .|. fromIntegral lsb
  pure (4 * (2048 - f))
