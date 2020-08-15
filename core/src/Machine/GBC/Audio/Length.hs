{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Audio.Length
  ( Length,
    newLength,
    initLength,
    powerOffLength,
    reloadLength,
    extraClocks,
    clockLength,
  )
where

import Control.Monad
import Data.Bits
import Data.IORef
import Data.Word
import Machine.GBC.Audio.Common
import Machine.GBC.Primitive

data Length = Length
  { bitMask :: !Word8,
    reloadValue :: !Int,
    counter :: !Counter,
    frozen :: !(IORef Bool) -- Has the length counter clocked to 0 and not been reloaded?
  }

newLength :: Word8 -> IO Length
newLength bitMask = do
  let reloadValue = 1 + fromIntegral bitMask
  counter <- newCounter (fromIntegral bitMask)
  frozen <- newIORef False -- Set when the counter reaches 0 to prevent further clocking.
  pure Length {..}

initLength :: Length -> FrameSequencerOutput -> Bool -> IO ()
initLength Length {..} frameSequencer enabled = do
  -- Unfreeze the length counter. If it is 0 then it will wrap around to bitMask
  -- on the next clock, so unfreezing the length counter effectively sets it to
  -- (bitMask + 1).  That's 64 for channels 1, 2, and 4, and 256 for channel 3.
  writeIORef frozen False
  v <- getCounter counter
  -- Quirk: If we are enabling the length counter, and it is currently 0, and
  -- the last frame sequencer step clocked the length, then clock the length
  -- again.
  when (enabled && (lastStepClockedLength frameSequencer) && v == 0) $
    updateCounter counter 1 (pure 0)

powerOffLength :: Length -> IO ()
powerOffLength Length {..} = do
  writeIORef frozen True
  reloadCounter counter 0

reloadLength :: Length -> Word8 -> IO ()
reloadLength Length {..} register =
  let len = fromIntegral (negate (register .&. bitMask) .&. bitMask)
   in do
        unless (len == 0) $ writeIORef frozen False
        reloadCounter counter len

-- Quirk: Sometimes, when the next frame sequencer step does not clock the
-- length counter, the length counter is immediately clocked anyway. This
-- function should be invoked to perform the extra length clocking.
extraClocks :: Length -> FrameSequencerOutput -> IO () -> IO ()
extraClocks Length {..} frameSequencer action = do
  isFrozen <- readIORef frozen
  when (lastStepClockedLength frameSequencer && not isFrozen) $
    updateCounter counter 1 $ do
      writeIORef frozen True
      0 <$ action

{-# INLINE clockLength #-}
clockLength :: Length -> IO () -> IO ()
clockLength Length {..} action = updateCounter counter 1 $ do
  writeIORef frozen True
  0 <$ action
