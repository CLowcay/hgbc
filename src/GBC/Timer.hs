{-# LANGUAGE RecordWildCards #-}
module GBC.Timer
  ( TimerState
  , initTimerState
  , HasTimer(..)
  , timerRegisters
  , updateTimer
  )
where

import           Data.IORef
import           GBC.Memory
import           GBC.CPU
import           GBC.Registers
import           Common
import           Control.Monad.Reader
import           Data.Word
import           Data.Bits

-- | Number of clocks until the next timer tick
data TimerState = TimerState {
    clockCounter :: IORef Word16
  , timerCounter :: IORef Word16
}

-- | Create the initial timer state.
initTimerState :: IO TimerState
initTimerState = TimerState <$> newIORef 0 <*> newIORef 0

class HasMemory env => HasTimer env where
  forTimerState :: env -> TimerState

-- | Select the relevant bits from the timer state given the low 2 bits of the
-- TAC register.
timerModulus :: Word8 -> Word16
timerModulus 0 = 1024
timerModulus 1 = 16
timerModulus 2 = 64
timerModulus 3 = 256
timerModulus x = error ("Unknown clock control pattern " ++ formatHex x)

-- | Check the timer stop flag for the TAC register.
timerStarted :: Word8 -> Bool
timerStarted = (`testBit` 2)

-- | Prepare a status report on the timer registers.
timerRegisters :: HasTimer env => ReaderT env IO [RegisterInfo]
timerRegisters = do
  tac <- readByte TAC
  sequence
    [ RegisterInfo DIV "DIV" <$> readByte DIV <*> pure []
    , RegisterInfo TIMA "TIMA" <$> readByte TIMA <*> pure []
    , pure (RegisterInfo TAC "TAC" tac (decodeTAC tac))
    ]
 where
  decodeTAC tac =
    let modulus   = timerModulus (tac .&. 3)
        frequency = (4 * 1024 * 1024 :: Int) `div` fromIntegral modulus
    in  [("Frequency", show frequency), ("Timer Started", show (timerStarted tac) ++ "Hz")]

-- | Update the timer state.
{-# INLINABLE updateTimer #-}
updateTimer :: HasTimer env => Int -> ReaderT env IO ()
updateTimer advance = do
  -- Update the internal clock count
  TimerState {..} <- asks forTimerState
  clocks          <- liftIO (readIORef clockCounter)
  let clocks' = clocks + fromIntegral advance
  liftIO (writeIORef clockCounter clocks')

  -- Update the DIV register if required.
  when (clocks .&. 0xFF00 /= clocks' .&. 0xFF00)
    $ writeByte DIV (fromIntegral (clocks' `unsafeShiftR` 8) :: Word8)

  -- Update the TIMA register
  tac <- readByte TAC
  when (timerStarted tac) $ do
    timer <- liftIO (readIORef timerCounter)
    -- liftIO (print timer)
    let (timerAdvance, timer') = (timer + fromIntegral advance) `divMod` timerModulus (tac .&. 3)
    liftIO (writeIORef timerCounter timer')

    -- Update required
    when (timerAdvance > 0) $ do
      tima <- readByte TIMA
      let tima' = fromIntegral tima + timerAdvance
      if tima' < 0xFF
        then writeByte TIMA (fromIntegral tima')
        else do
          -- Handle overflow by loading from TMA
          raiseInterrupt 2
          tma <- readByte TMA
          if tma == 0xFF
            then writeByte TIMA 0xFF
            else writeByte TIMA (tma + ((fromIntegral timerAdvance - 1) `mod` (0xFF - tma)))
