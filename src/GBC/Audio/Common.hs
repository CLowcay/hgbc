module GBC.Audio.Common
  ( Channel(..)
  , FrameSequencerOutput(..)
  , noFrameSequencerOutput
  , flagTrigger
  , flagLength
  , getFrequency
  , updateFrequency
  , flagChannel1Enable
  , flagChannel2Enable
  , flagChannel3Enable
  , flagChannel4Enable
  , updateStatus
  )
where

import           Common
import           Data.Bits
import           Data.Word
import           GBC.Primitive

flagTrigger, flagLength :: Word8
flagTrigger = 0x80
flagLength = 0x40

data FrameSequencerOutput = FrameSequencerOutput {
    lengthClock   :: !Bool
  , envelopeClock :: !Bool
  , sweepClock    :: !Bool
}

noFrameSequencerOutput :: FrameSequencerOutput
noFrameSequencerOutput = FrameSequencerOutput False False False

class Channel channel where
  getOutput           :: channel -> IO Int
  disable             :: channel -> IO ()
  getStatus           :: channel -> IO Bool
  frameSequencerClock :: channel -> FrameSequencerOutput -> IO ()
  masterClock         :: channel -> Int -> IO ()
  getPorts            :: channel -> [(Int, Port Word8)]

getFrequency :: Word8 -> Word8 -> Int
getFrequency register3 register4 =
  let lsb = register3
      msb = register4
  in  ((fromIntegral msb .&. 0x07) .<<. 8) .|. fromIntegral lsb

updateFrequency :: Port Word8 -> Port Word8 -> Int -> IO ()
updateFrequency port3 port4 frequency = do
  let lsb = fromIntegral (frequency .&. 0xFF)
  let msb = fromIntegral ((frequency `unsafeShiftR` 8) .&. 0x07)
  register4 <- directReadPort port4
  directWritePort port3 lsb
  directWritePort port4 ((register4 .&. 0xF8) .|. msb)

flagChannel1Enable, flagChannel2Enable, flagChannel3Enable, flagChannel4Enable :: Word8
flagChannel1Enable = 0x01
flagChannel2Enable = 0x02
flagChannel3Enable = 0x04
flagChannel4Enable = 0x08

updateStatus :: Port Word8 -> Word8 -> Bool -> IO ()
updateStatus port52 flag enabled = do
  register52 <- directReadPort port52
  directWritePort port52 (if enabled then register52 .|. flag else register52 .&. complement flag)
