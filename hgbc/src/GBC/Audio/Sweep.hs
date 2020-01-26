{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Sweep
  ( Sweep
  , newSweep
  , initSweep
  , clockSweep
  , flagNegate
  , hasPerformedSweepCalculationInNegateMode
  )
where

import           Common
import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Primitive
import           GBC.Primitive.UnboxedRef

data Sweep = Sweep {
    enable       :: !(IORef Bool)
  , hasNegated   :: !(IORef Bool)
  , port3        :: !(Port Word8)
  , port4        :: !(Port Word8)
  , frequencyRef :: !(UnboxedRef Int)
  , counter      :: !Counter
}

newSweep :: Port Word8 -> Port Word8 -> IO Sweep
newSweep port3 port4 = do
  enable       <- newIORef False
  hasNegated   <- newIORef False
  frequencyRef <- newUnboxedRef 0
  counter      <- newCounter 7
  pure Sweep { .. }

initSweep :: Sweep -> Int -> Word8 -> IO () -> IO ()
initSweep sweep@Sweep {..} frequency0 register disableIO = do
  writeIORef hasNegated False
  writeUnboxedRef frequencyRef frequency0
  reloadCounter counter (getPeriod register)
  writeIORef enable (getPeriod register /= 0 || getShift register /= 0)
  when (getShift register /= 0) $ void $ overflowCheck sweep register disableIO

nextFrequency :: Int -> Int -> Bool -> Int
nextFrequency frequency0 sweepShift False = frequency0 + (frequency0 .>>. sweepShift)
nextFrequency frequency0 sweepShift True  = frequency0 - (frequency0 .>>. sweepShift)

overflowCheck :: Sweep -> Word8 -> IO () -> IO Int
overflowCheck Sweep {..} register disableIO = do
  frequency <- readUnboxedRef frequencyRef
  let isNegate   = isFlagSet flagNegate register
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
