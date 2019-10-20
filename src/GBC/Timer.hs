{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module GBC.Timer
  ( TimerState
  , initTimerState
  , HasTimerState(..)
  , UsesTimer
  , updateTimer
  )
where

import           Data.IORef
import           GBC.Memory
import           GBC.CPU
import           Common
import           Control.Exception              ( assert )
import           Control.Monad.Reader
import           Data.Word
import           Data.Bits

-- | Number of clocks until the next timer tick
type TimerState = IORef Word16

-- | Create the initial timer state.
initTimerState :: IO TimerState
initTimerState = newIORef 0

class HasTimerState env where
  forTimerState :: env -> TimerState

type UsesTimer env m = (HasTimerState env, UsesMemory env m)

pattern DIV :: Word16
pattern DIV = 0xFF04

pattern TIMA :: Word16
pattern TIMA = 0xFF05

pattern TMA :: Word16
pattern TMA = 0xFF06

pattern TAC :: Word16
pattern TAC = 0xFF07

-- | Select the relevant bits from the timer state given the low 2 bits of the
-- TAC register.
clockControlMask :: Word8 -> Word16
clockControlMask 0 = 0xFC00
clockControlMask 1 = 0xFFF0
clockControlMask 2 = 0xFFC0
clockControlMask 3 = 0xFF00
clockControlMask x = error $ "Unknown clock control pattern " ++ formatHex x

-- | Check the timer stop flag for the TAC register.
testTimerStop :: Word8 -> Bool
testTimerStop = (`testBit` 2)

-- | Update the timer state.
updateTimer :: UsesTimer env m => Int -> ReaderT env m ()
updateTimer advance = do
  -- Update the internal clock count
  timer  <- asks forTimerState
  clocks <- liftIO $ readIORef timer
  let clocks' = clocks + fromIntegral advance
  liftIO $ writeIORef timer clocks'

  -- Update the DIV register if required.
  when (clocks .&. 0xFF00 /= clocks' .&. 0xFF00)
    $ writeByte DIV (fromIntegral (clocks' `shiftR` 8) :: Word8)

  -- Update the TIMA register
  when (clocks .&. 0xFFF0 /= clocks' .&. 0xFFF0) $ do
    tac <- readByte TAC
    let mask = clockControlMask $ tac .&. 0x03

    -- Update required
    when (not (testTimerStop tac) && (clocks .&. mask /= clocks' .&. mask)) $ do
      assert
        (abs (fromIntegral (clocks .&. mask) - fromIntegral (clocks' .&. mask)) == (1 :: Int))
        (pure ())

      tima <- readByte TIMA
      if tima < 0xFF
        then writeByte TIMA $ tima + 1
        else do
          -- Handle overflow by loading from TMA
          tma <- readByte TMA
          writeByte TIMA tma
          raiseInterrupt 2
