{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Length
  ( Length
  , newLength
  , initLength
  , reloadLength
  , extraClocks
  , clockLength
  )
where

import           GBC.Primitive
import           Data.Word
import           Data.IORef
import           Data.Bits
import           Control.Monad

data Length = Length {
    bitMask            :: !Word8
  , counter            :: !Counter
  , frozen             :: !(IORef Bool) -- Has the length counter clocked to 0 and not been reloaded?
  , clockedOnLastFrame :: !(IORef Bool) -- Was the length counter clocked on the last audio frame sequencer clock?
}

newLength :: Word8 -> IO Length
newLength bitMask = do
  counter            <- newCounter
  frozen             <- newIORef False
  clockedOnLastFrame <- newIORef False
  pure Length { .. }

initLength :: Length -> Bool -> IO ()
initLength Length {..} enabled = do
  writeIORef frozen False
  v               <- getCounter counter
  needExtraClocks <- readIORef clockedOnLastFrame
  when (enabled && needExtraClocks && v == fromIntegral bitMask) $ updateCounter counter 1 (pure 0)

reloadLength :: Length -> Word8 -> IO ()
reloadLength Length {..} register =
  let len = negate (fromIntegral (register .&. bitMask))
  in  do
        when (len /= 0) $ writeIORef frozen False
        reloadCounter counter ((len - 1) .&. fromIntegral bitMask)

extraClocks :: Length -> IO () -> IO ()
extraClocks Length {..} action = do
  needExtraClocks <- readIORef clockedOnLastFrame
  when needExtraClocks $ do
    isFrozen <- readIORef frozen
    unless isFrozen $ updateCounter counter 1 $ do
      writeIORef frozen True
      fromIntegral bitMask <$ action

{-# INLINE clockLength #-}
clockLength :: Length -> Bool -> Bool -> IO () -> IO ()
clockLength Length {..} doClock lengthEnabled action = do
  writeIORef clockedOnLastFrame doClock
  when (doClock && lengthEnabled) $ updateCounter counter 1 $ do
    writeIORef frozen True
    fromIntegral bitMask <$ action
