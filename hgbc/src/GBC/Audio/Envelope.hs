{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Envelope
  ( Envelope
  , newEnvelope
  , initEnvelope
  , clockEnvelope
  , envelopeVolume
  )
where

import           Common
import           Control.Monad
import           Data.Bits
import           Data.Word
import           GBC.Audio.Common
import           GBC.Primitive
import           GBC.Primitive.UnboxedRef

data Envelope = Envelope {
    volumeRef        :: !(UnboxedRef Int)
  , volumeDeltaRef   :: !(UnboxedRef Int)
  , envelopePeriodRef:: !(UnboxedRef Int)
  , envelopeCounter  :: !Counter
}

newEnvelope :: IO Envelope
newEnvelope = do
  volumeRef         <- newUnboxedRef 0
  volumeDeltaRef    <- newUnboxedRef 1
  envelopePeriodRef <- newUnboxedRef 0
  envelopeCounter   <- newCounter 7
  pure Envelope { .. }

initEnvelope :: Envelope -> Word8 -> FrameSequencerOutput -> IO ()
initEnvelope Envelope {..} register step = do
  let envelopePeriod = getEnvelopePeriod register
  -- Quirk: If the next frame sequencer step will clock the envelope, then init
  -- it to one greater than what it should be.
  reloadCounter envelopeCounter
                (if nextStepWillClockEnvelope step then envelopePeriod + 1 else envelopePeriod)
  writeUnboxedRef envelopePeriodRef envelopePeriod
  writeUnboxedRef volumeDeltaRef    (getVolumeDelta register)
  writeUnboxedRef volumeRef         (getVolume register)

clockEnvelope :: Envelope -> IO ()
clockEnvelope Envelope {..} = do
  envelopePeriod <- readUnboxedRef envelopePeriodRef
  unless (envelopePeriod == 0) $ updateCounter envelopeCounter 1 $ do
    volume      <- readUnboxedRef volumeRef
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
