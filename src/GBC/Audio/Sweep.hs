{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Sweep
  ( Sweep
  , newSweep
  , initSweep
  , clockSweep
  )
where

import           Common
import           Data.IORef
import           GBC.Primitive
import           Control.Monad
import           Data.Word
import           Data.Bits

data Sweep = Sweep {
    enable :: IORef Bool
  , frequencyRef :: IORef Int
  , counter :: Counter
}

newSweep :: IO Sweep
newSweep = do
  enable       <- newIORef False
  frequencyRef <- newIORef 0
  counter      <- newCounter
  pure Sweep { .. }

initSweep :: Sweep -> Int -> Word8 -> IO ()
initSweep Sweep {..} frequency0 register = do
  writeIORef frequencyRef frequency0
  reloadCounter counter (getPeriod register)
  writeIORef enable (getPeriod register /= 0 || getShift register /= 0)

nextFrequency :: Int -> Int -> Bool -> Int
nextFrequency frequency0 sweepShift False = frequency0 + (frequency0 `unsafeShiftR` sweepShift)
nextFrequency frequency0 sweepShift True  = frequency0 - (frequency0 `unsafeShiftR` sweepShift)

overflowCheck :: Sweep -> Word8 -> IO Int
overflowCheck Sweep {..} register = do
  frequency <- readIORef frequencyRef
  let frequency' = nextFrequency frequency (getShift register) (isFlagSet flagNegate register)
  if frequency' > 2047 then frequency <$ writeIORef enable False else pure frequency'

clockSweep :: Sweep -> Word8 -> IO (Maybe Int)
clockSweep sweep@Sweep {..} register = do
  isEnabled <- readIORef enable
  if not isEnabled || getPeriod register == 0
    then pure Nothing
    else do
      frequency' <- overflowCheck sweep register
      if getShift register == 0
        then pure Nothing
        else do
          writeIORef frequencyRef frequency'
          void (overflowCheck sweep register)
          pure (Just frequency')

flagNegate :: Word8
flagNegate = 0x08

getPeriod :: Word8 -> Int
getPeriod register = fromIntegral register `unsafeShiftR` 4

getShift :: Word8 -> Int
getShift register = fromIntegral register .&. 0x07

