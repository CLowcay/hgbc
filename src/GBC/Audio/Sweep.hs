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
import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Memory
import           GBC.Primitive

data Sweep = Sweep {
    enable       :: IORef Bool
  , hasNegated   :: IORef Bool
  , baseRegister :: Word16
  , frequencyRef :: IORef Int
  , counter      :: Counter
}

newSweep :: Word16 -> IO Sweep
newSweep baseRegister = do
  enable       <- newIORef False
  hasNegated   <- newIORef False
  frequencyRef <- newIORef 0
  counter      <- newCounter
  pure Sweep { .. }

initSweep :: Sweep -> Int -> Word8 -> IO () -> IO ()
initSweep sweep@Sweep {..} frequency0 register disableIO = do
  writeIORef hasNegated   False
  writeIORef frequencyRef frequency0
  reloadCounter counter ((getPeriod register - 1) .&. 7)
  writeIORef enable (getPeriod register /= 0 || getShift register /= 0)
  when (getShift register /= 0) $ void $ overflowCheck sweep register disableIO

nextFrequency :: Int -> Int -> Bool -> Int
nextFrequency frequency0 sweepShift False = frequency0 + (frequency0 `unsafeShiftR` sweepShift)
nextFrequency frequency0 sweepShift True  = frequency0 - (frequency0 `unsafeShiftR` sweepShift)

overflowCheck :: Sweep -> Word8 -> IO () -> IO Int
overflowCheck Sweep {..} register disableIO = do
  frequency <- readIORef frequencyRef
  let isNegate   = isFlagSet flagNegate register
  let frequency' = nextFrequency frequency (getShift register) isNegate
  when isNegate $ writeIORef hasNegated True
  if frequency' > 2047 then frequency <$ disableIO else pure frequency'

clockSweep :: HasMemory env => Sweep -> Word8 -> IO () -> ReaderT env IO ()
clockSweep sweep@Sweep {..} register disableIO = updateCounter counter 1 $ do
  isEnabled <- liftIO $ readIORef enable
  when (isEnabled && getPeriod register /= 0) $ do
    frequency' <- liftIO $ overflowCheck sweep register disableIO
    when (getShift register /= 0) $ do
      liftIO $ writeIORef frequencyRef frequency'
      updateFrequency baseRegister frequency'
      void $ liftIO $ overflowCheck sweep register disableIO
  pure ((getPeriod register - 1) .&. 7)

hasPerformedSweepCalculationInNegateMode :: Sweep -> IO Bool
hasPerformedSweepCalculationInNegateMode Sweep {..} = readIORef hasNegated

flagNegate :: Word8
flagNegate = 0x08

getPeriod :: Word8 -> Int
getPeriod register = fromIntegral register `unsafeShiftR` 4

getShift :: Word8 -> Int
getShift register = fromIntegral register .&. 0x07

