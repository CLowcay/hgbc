module GBC.Audio.Common
  ( Channel(..)
  , FrameSequencerOutput(..)
  , noFrameSequencerOutput
  , flagTrigger
  , flagLength
  , getPTPeriod
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.Word
import           GBC.Memory

flagTrigger, flagLength :: Word8
flagTrigger = 0x80
flagLength = 0x40

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

getPTPeriod :: HasMemory env => Word16 -> ReaderT env IO Word16
getPTPeriod base = do
  lsb <- readByte base
  msb <- readByte (base + 1)
  pure $ negate ((fromIntegral lsb `unsafeShiftL` 5) .|. (fromIntegral msb `unsafeShiftL` 13))
