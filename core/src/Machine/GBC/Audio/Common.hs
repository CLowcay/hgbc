module Machine.GBC.Audio.Common
  ( Channel (..),
    FrameSequencerOutput (..),
    lastStepClockedLength,
    nextStepWillClockEnvelope,
    isLengthClockingStep,
    isEnvelopeClockingStep,
    isSweepClockingStep,
    flagMasterPower,
    flagTrigger,
    flagLength,
    getFrequency,
    updateFrequency,
    flagChannel1Enable,
    flagChannel2Enable,
    flagChannel3Enable,
    flagChannel4Enable,
    updateStatus,
    newAudioPort,
    newAudioPortWithReadMask,
  )
where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port
import Machine.GBC.Util (isFlagSet, (.<<.))

flagTrigger, flagLength :: Word8
flagTrigger = 0x80
flagLength = 0x40

newtype FrameSequencerOutput = FrameSequencerOutput Int deriving (Eq, Ord, Show)

{-# INLINE lastStepClockedLength #-}
lastStepClockedLength :: FrameSequencerOutput -> Bool
lastStepClockedLength (FrameSequencerOutput i) = i .&. 1 == 0

{-# INLINE nextStepWillClockEnvelope #-}
nextStepWillClockEnvelope :: FrameSequencerOutput -> Bool
nextStepWillClockEnvelope (FrameSequencerOutput i) = i == 6

{-# INLINE isLengthClockingStep #-}
isLengthClockingStep :: FrameSequencerOutput -> Bool
isLengthClockingStep (FrameSequencerOutput i) = i .&. 1 == 0

{-# INLINE isEnvelopeClockingStep #-}
isEnvelopeClockingStep :: FrameSequencerOutput -> Bool
isEnvelopeClockingStep (FrameSequencerOutput i) = i == 7

{-# INLINE isSweepClockingStep #-}
isSweepClockingStep :: FrameSequencerOutput -> Bool
isSweepClockingStep (FrameSequencerOutput i) = i .&. 3 == 2

class Channel channel where
  getOutput :: channel -> IO Int
  disable :: channel -> IO ()
  powerOff :: channel -> IO ()
  getStatus :: channel -> IO Bool
  frameSequencerClock :: channel -> FrameSequencerOutput -> IO ()
  masterClock :: channel -> Int -> IO ()
  getPorts :: channel -> [(Int, Port)]
  directReadPorts :: channel -> IO (Word8, Word8, Word8, Word8, Word8)

{-# INLINE getFrequency #-}
getFrequency :: Word8 -> Word8 -> Int
getFrequency register3 register4 =
  let lsb = register3
      msb = register4
   in ((fromIntegral msb .&. 0x07) .<<. 8) .|. fromIntegral lsb

updateFrequency :: Port -> Port -> Int -> IO ()
updateFrequency port3 port4 frequency = do
  let lsb = fromIntegral (frequency .&. 0xFF)
  let msb = fromIntegral ((frequency `unsafeShiftR` 8) .&. 0x07)
  register4 <- Port.readDirect port4
  Port.writeDirect port3 lsb
  Port.writeDirect port4 ((register4 .&. 0xF8) .|. msb)

flagMasterPower,
  flagChannel1Enable,
  flagChannel2Enable,
  flagChannel3Enable,
  flagChannel4Enable ::
    Word8
flagChannel1Enable = 0x01
flagChannel2Enable = 0x02
flagChannel3Enable = 0x04
flagChannel4Enable = 0x08
flagMasterPower = 0x80

updateStatus :: Port -> Word8 -> Bool -> IO ()
updateStatus port52 flag enabled = do
  register52 <- Port.readDirect port52
  Port.writeDirect port52 (if enabled then register52 .|. flag else register52 .&. complement flag)

-- | Create a new port.
newAudioPort ::
  Port ->
  -- | Initial value.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newAudioPort port52 value0 portWriteMask portNotify = Port.new value0 portWriteMask $ \old new -> do
  nr52 <- Port.readDirect port52
  if isFlagSet flagMasterPower nr52 then portNotify old new else pure old

-- | Create a new port.
newAudioPortWithReadMask ::
  Port ->
  -- | Initial value.
  Word8 ->
  -- | Read mask.  1 indicates that the bit will always read as 1.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newAudioPortWithReadMask port52 value0 portReadMask portWriteMask portNotify =
  Port.newWithReadMask value0 portReadMask portWriteMask $ \old new -> do
    nr52 <- Port.readDirect port52
    if isFlagSet flagMasterPower nr52 then portNotify old new else pure old
