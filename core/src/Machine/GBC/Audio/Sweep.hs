{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Audio.Sweep
  ( Sweep,
    newSweep,
    initSweep,
    clockSweep,
    flagNegate,
    hasPerformedSweepCalculationInNegateMode,
  )
where

import Control.Monad (void, when)
import Data.Bits (Bits ((.&.)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Machine.GBC.Audio.Common (updateFrequency)
import Machine.GBC.Primitive
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import Machine.GBC.Util (isFlagSet, (.>>.))

data Sweep = Sweep
  { enable :: !(IORef Bool),
    hasNegated :: !(IORef Bool),
    port3 :: !Port,
    port4 :: !Port,
    frequencyRef :: !(UnboxedRef Int),
    counter :: !Counter
  }

newSweep :: Port -> Port -> IO Sweep
newSweep port3 port4 = do
  enable <- newIORef False
  hasNegated <- newIORef False
  frequencyRef <- newUnboxedRef 0
  counter <- newCounter 7
  pure Sweep {..}

initSweep :: Sweep -> Int -> Word8 -> IO () -> IO ()
initSweep sweep@Sweep {..} frequency0 register disableIO = do
  writeIORef hasNegated False
  writeUnboxedRef frequencyRef frequency0
  reloadCounter counter (getPeriod register)
  writeIORef enable (getPeriod register /= 0 || getShift register /= 0)
  when (getShift register /= 0) $ void $ overflowCheck sweep register disableIO

nextFrequency :: Int -> Int -> Bool -> Int
nextFrequency frequency0 sweepShift False = frequency0 + (frequency0 .>>. sweepShift)
nextFrequency frequency0 sweepShift True = frequency0 - (frequency0 .>>. sweepShift)

overflowCheck :: Sweep -> Word8 -> IO () -> IO Int
overflowCheck Sweep {..} register disableIO = do
  frequency <- readUnboxedRef frequencyRef
  let isNegate = isFlagSet flagNegate register
  let frequency' = nextFrequency frequency (getShift register) isNegate
  when isNegate $ writeIORef hasNegated True
  if frequency' > 2047 then frequency <$ disableIO else pure frequency'

clockSweep :: Sweep -> Word8 -> IO () -> IO ()
clockSweep sweep@Sweep {..} register disableIO = updateCounter counter 1 $ do
  isEnabled <- readIORef enable
  when (isEnabled && getPeriod register /= 0) $ do
    frequency' <- overflowCheck sweep register disableIO
    when (getShift register /= 0) $ do
      writeUnboxedRef frequencyRef frequency'
      updateFrequency port3 port4 frequency'
      void $ overflowCheck sweep register disableIO
  pure (getPeriod register)

hasPerformedSweepCalculationInNegateMode :: Sweep -> IO Bool
hasPerformedSweepCalculationInNegateMode Sweep {..} = readIORef hasNegated

flagNegate :: Word8
flagNegate = 0x08

getPeriod :: Word8 -> Int
getPeriod register = (fromIntegral register .>>. 4) .&. 0x07

getShift :: Word8 -> Int
getShift register = fromIntegral register .&. 0x07
