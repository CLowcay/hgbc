{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Length
  ( Length
  , newLength
  , initLength
  , powerOffLength
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
  , reloadValue        :: !Int
  , counter            :: !Counter
  , frozen             :: !(IORef Bool) -- Has the length counter clocked to 0 and not been reloaded?
  , clockedOnLastFrame :: !(IORef Bool) -- Was the length counter clocked on the last audio frame sequencer clock?
}

newLength :: Word8 -> IO Length
newLength bitMask = do
  let reloadValue = 1 + fromIntegral bitMask
  counter            <- newCounter (fromIntegral bitMask)
  frozen             <- newIORef False -- Set when the counter reaches 0 to prevent further clocking.
  clockedOnLastFrame <- newIORef False -- Remember if the last frame-sequencer step clocked the length counter.
  pure Length { .. }

initLength :: Length -> Bool -> IO ()
initLength Length {..} enabled = do
  -- Unfreeze the length counter. If it is 0 then it will wrap around to bitMask
  -- on the next clock, so unfreezing the length counter effectively sets it to
  -- (bitMask + 1).  That's 64 for channels 1, 2, and 4, and 256 for channel 3.
  writeIORef frozen False
  v               <- getCounter counter
  needExtraClocks <- readIORef clockedOnLastFrame
  -- Quirk: If we are enabling the length counter, and it is currently 0, and
  -- the last frame sequencer step clocked the length, then clock the length
  -- again.
  when (enabled && needExtraClocks && v == 0) $ updateCounter counter 1 (pure 0)

powerOffLength :: Length -> IO ()
powerOffLength Length {..} = do
  writeIORef clockedOnLastFrame False
  writeIORef frozen             True
  reloadCounter counter 0

reloadLength :: Length -> Word8 -> IO ()
reloadLength Length {..} register =
  let len = fromIntegral (negate (register .&. bitMask) .&. bitMask)
  in  do
        unless (len == 0) $ writeIORef frozen False
        reloadCounter counter len

extraClocks :: Length -> IO () -> IO ()
extraClocks Length {..} action = do
  needExtraClocks <- readIORef clockedOnLastFrame
  isFrozen        <- readIORef frozen
  when (needExtraClocks && not isFrozen) $ updateCounter counter 1 $ do
    writeIORef frozen True
    0 <$ action

{-# INLINE clockLength #-}
clockLength :: Length -> Bool -> Bool -> IO () -> IO ()
clockLength Length {..} doClock lengthEnabled action = do
  writeIORef clockedOnLastFrame doClock
  when (doClock && lengthEnabled) $ updateCounter counter 1 $ do
    writeIORef frozen True
    0 <$ action
