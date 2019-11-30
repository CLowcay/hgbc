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
import           Data.IORef
import           Data.Word
import           GBC.Primitive

data Envelope = Envelope {
    volumeRef        :: IORef Int
  , volumeDeltaRef   :: IORef Int
  , envelopePeriodRef:: IORef Int
  , envelopeCounter  :: Counter
}

newEnvelope :: IO Envelope
newEnvelope = do
  volumeRef         <- newIORef 0
  volumeDeltaRef    <- newIORef 1
  envelopePeriodRef <- newIORef 0
  envelopeCounter   <- newCounter
  pure Envelope { .. }

initEnvelope :: Envelope -> Word8 -> IO ()
initEnvelope Envelope {..} register = do
  let envelopePeriod = getEnvelopePeriod register
  reloadCounter envelopeCounter envelopePeriod
  writeIORef envelopePeriodRef envelopePeriod
  writeIORef volumeDeltaRef    (getVolumeDelta register)
  writeIORef volumeRef         (getVolume register)

clockEnvelope :: Envelope -> IO ()
clockEnvelope Envelope {..} = do
  envelopePeriod <- readIORef envelopePeriodRef
  when (envelopePeriod /= 0) $ updateCounter envelopeCounter 1 $ do
    volume      <- readIORef volumeRef
    volumeDelta <- readIORef volumeDeltaRef
    writeIORef volumeRef (((volume + volumeDelta) `min` 15) `max` 0)
    pure envelopePeriod

envelopeVolume :: Envelope -> IO Int
envelopeVolume Envelope {..} = readIORef volumeRef

flagDirection :: Word8
flagDirection = 0x08

getVolume :: Word8 -> Int
getVolume register = fromIntegral register `unsafeShiftR` 4

getVolumeDelta :: Word8 -> Int
getVolumeDelta register = if isFlagSet flagDirection register then 1 else (-1)

getEnvelopePeriod :: Word8 -> Int
getEnvelopePeriod register = fromIntegral (register .&. 0x07)
