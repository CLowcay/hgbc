{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Audio.Envelope
  ( Envelope,
    newEnvelope,
    initEnvelope,
    clockEnvelope,
    envelopeVolume,
  )
where

import Control.Monad (unless)
import Data.Bits (Bits (..))
import Data.Word (Word8)
import Machine.GBC.Audio.Common (FrameSequencerOutput, nextStepWillClockEnvelope)
import Machine.GBC.Primitive.Counter (Counter)
import qualified Machine.GBC.Primitive.Counter as Counter
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import Machine.GBC.Util (isFlagSet, (.>>.))

data Envelope = Envelope
  { volumeRef :: !(UnboxedRef Int),
    volumeDeltaRef :: !(UnboxedRef Int),
    envelopePeriodRef :: !(UnboxedRef Int),
    envelopeCounter :: !Counter
  }

newEnvelope :: IO Envelope
newEnvelope = do
  volumeRef <- newUnboxedRef 0
  volumeDeltaRef <- newUnboxedRef 1
  envelopePeriodRef <- newUnboxedRef 0
  envelopeCounter <- Counter.new 7
  pure Envelope {..}

initEnvelope :: Envelope -> Word8 -> FrameSequencerOutput -> IO ()
initEnvelope Envelope {..} register step = do
  let envelopePeriod = getEnvelopePeriod register
  -- Quirk: If the next frame sequencer step will clock the envelope, then init
  -- it to one greater than what it should be.
  Counter.reload
    envelopeCounter
    (if nextStepWillClockEnvelope step then envelopePeriod + 1 else envelopePeriod)
  writeUnboxedRef envelopePeriodRef envelopePeriod
  writeUnboxedRef volumeDeltaRef (getVolumeDelta register)
  writeUnboxedRef volumeRef (getVolume register)

clockEnvelope :: Envelope -> IO ()
clockEnvelope Envelope {..} = do
  envelopePeriod <- readUnboxedRef envelopePeriodRef
  unless (envelopePeriod == 0) $
    Counter.update envelopeCounter 1 $ do
      volume <- readUnboxedRef volumeRef
      volumeDelta <- readUnboxedRef volumeDeltaRef
      writeUnboxedRef volumeRef $ ((volume + volumeDelta) `min` 15) `max` 0
      pure envelopePeriod

envelopeVolume :: Envelope -> IO Int
envelopeVolume Envelope {..} = readUnboxedRef volumeRef

flagDirection :: Word8
flagDirection = 0x08

getVolume :: Word8 -> Int
getVolume register = fromIntegral register .>>. 4

getVolumeDelta :: Word8 -> Int
getVolumeDelta register = if isFlagSet flagDirection register then 1 else (-1)

getEnvelopePeriod :: Word8 -> Int
getEnvelopePeriod register = fromIntegral (register .&. 0x07)
