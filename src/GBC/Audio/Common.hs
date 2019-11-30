{-# LANGUAGE TupleSections #-}
module GBC.Audio.Common
  ( Channel(..)
  , FrameSequencerOutput(..)
  , noFrameSequencerOutput
  , flagTrigger
  , flagLength
  , flagDirection
  , dutyCycleStates
  , getDutyCycle
  , getLengthPeriod
  , getVolume
  , getVolumeDelta
  , getEnvelopePeriod
  , dutyCycleOutput
  , getTimerPeriod
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Functor
import           Data.Word
import           GBC.Memory

flagTrigger, flagLength :: Word8
flagTrigger = 0x80
flagLength = 0x40

flagDirection :: Word8
flagDirection = 0x08

data FrameSequencerOutput = FrameSequencerOutput {
    lengthClock :: !Bool
  , envelopeClock :: !Bool
  , sweepClock :: !Bool
}

noFrameSequencerOutput :: FrameSequencerOutput
noFrameSequencerOutput = FrameSequencerOutput False False False

class Channel channel where
  getOutput :: HasMemory env => channel -> ReaderT env IO Int
  disable :: HasMemory env => channel -> ReaderT env IO ()
  trigger :: HasMemory env => channel -> ReaderT env IO ()
  frameSequencerClock :: HasMemory env => channel -> FrameSequencerOutput -> ReaderT env IO ()
  masterClock :: HasMemory env => channel -> Int -> ReaderT env IO ()
  writeX0 :: HasMemory env => channel -> ReaderT env IO ()
  writeX1 :: HasMemory env => channel -> ReaderT env IO ()
  writeX2 :: HasMemory env => channel -> ReaderT env IO ()
  writeX3 :: HasMemory env => channel -> ReaderT env IO ()

getDutyCycle :: Word8 -> Word8
getDutyCycle register = register `unsafeShiftR` 6

getLengthPeriod :: Word8 -> Int
getLengthPeriod register = negate (fromIntegral register .&. 0x3F) .&. 0x3F

getVolume :: Word8 -> Int
getVolume register = fromIntegral register `unsafeShiftR` 4

getVolumeDelta :: Word8 -> Int
getVolumeDelta register = if (isFlagSet flagDirection register) then 1 else (-1)

getEnvelopePeriod :: Word8 -> Int
getEnvelopePeriod register = fromIntegral (register .&. 0x07)

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
  lsb <- readByte base
  msb <- readByte (base + 1)
  let f = ((fromIntegral msb .&. 0x07) `unsafeShiftL` 8) .|. fromIntegral lsb
  pure (4 * (2048 - f))
