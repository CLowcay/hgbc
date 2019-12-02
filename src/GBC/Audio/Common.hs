module GBC.Audio.Common
  ( Channel(..)
  , FrameSequencerOutput(..)
  , noFrameSequencerOutput
  , flagTrigger
  , flagLength
  , getFrequency
  , updateFrequency
  )
where

import           Control.Monad.Reader
import           Data.Word
import           Data.Bits
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
  getStatus :: HasMemory env => channel -> ReaderT env IO Bool
  trigger :: HasMemory env => channel -> ReaderT env IO ()
  frameSequencerClock :: HasMemory env => channel -> FrameSequencerOutput -> ReaderT env IO ()
  masterClock :: HasMemory env => channel -> Int -> ReaderT env IO ()
  writeX0 :: HasMemory env => channel -> ReaderT env IO ()
  writeX1 :: HasMemory env => channel -> ReaderT env IO ()
  writeX2 :: HasMemory env => channel -> ReaderT env IO ()
  writeX3 :: HasMemory env => channel -> ReaderT env IO ()
  writeX4 :: HasMemory env => channel -> ReaderT env IO ()

getFrequency :: HasMemory env => Word16 -> ReaderT env IO Int
getFrequency base = do
  lsb <- readByte (base + 3)
  msb <- readByte (base + 4)
  pure (((fromIntegral msb .&. 0x07) `unsafeShiftL` 8) .|. fromIntegral lsb)

updateFrequency :: HasMemory env => Word16 -> Int -> ReaderT env IO ()
updateFrequency base frequency = do
  let lsb = fromIntegral (frequency .&. 0xFF)
  let msb = fromIntegral ((frequency `unsafeShiftR` 8) .&. 0x07)
  register4 <- readByte (base + 4)
  writeByte (base + 3) lsb
  writeByte (base + 4) ((register4 .&. 0xF8) .|. msb)
