{-# LANGUAGE RecordWildCards #-}
module Machine.GBC.Timer
  ( TimerState
  , initTimerState
  , timerPorts
  , timerRegisters
  , updateTimer
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.Word
import           Machine.GBC.CPU.Interrupts
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Registers
import           Machine.GBC.Util

-- | The current state of the timer hardware.
data TimerState = TimerState {
    clockCounter :: !(UnboxedRef Word16)
  , timerCounter :: !(UnboxedRef Word16)
  , portDIV      :: !(Port Word8)
  , portTIMA     :: !(Port Word8)
  , portTMA      :: !(Port Word8)
  , portTAC      :: !(Port Word8)
  , portIF       :: !(Port Word8)
}

-- | Create the initial timer state.
initTimerState :: Port Word8 -> IO TimerState
initTimerState portIF = do
  clockCounter <- newUnboxedRef 0
  timerCounter <- newUnboxedRef 0
  portDIV      <- newPort 0x00 0xFF alwaysUpdate
  portTIMA     <- newPort 0x00 0xFF alwaysUpdate
  portTMA      <- newPort 0x00 0xFF alwaysUpdate
  portTAC      <- newPort 0x00 0xFF alwaysUpdate
  pure TimerState { .. }

timerPorts :: TimerState -> [(Word16, Port Word8)]
timerPorts TimerState {..} = [(DIV, portDIV), (TIMA, portTIMA), (TMA, portTMA), (TAC, portTAC)]

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
timerRegisters :: TimerState -> IO [RegisterInfo]
timerRegisters TimerState {..} = do
  tac <- readPort portTAC
  sequence
    [ RegisterInfo DIV "DIV" <$> readPort portDIV <*> pure []
    , RegisterInfo TIMA "TIMA" <$> readPort portTIMA <*> pure []
    , pure (RegisterInfo TAC "TAC" tac (decodeTAC tac))
    ]
 where
  decodeTAC tac =
    let modulus   = timerModulus (tac .&. 3)
        frequency = (4 * 1024 * 1024 :: Int) `div` fromIntegral modulus
    in  [("Frequency", show frequency), ("Timer Started", show (timerStarted tac) ++ "Hz")]

-- | Update the timer state.
updateTimer :: TimerState -> Int -> IO ()
updateTimer TimerState {..} advance = do
  -- Update the internal clock count
  clocks <- readUnboxedRef clockCounter
  let clocks' = clocks + fromIntegral advance
  writeUnboxedRef clockCounter clocks'

  -- Update the DIV register if required.
  when (clocks .&. 0xFF00 /= clocks' .&. 0xFF00)
    $ directWritePort portDIV (fromIntegral (clocks' .>>. 8) :: Word8)

  -- Update the TIMA register
  tac <- directReadPort portTAC
  when (timerStarted tac) $ do
    timer <- readUnboxedRef timerCounter
    let (timerAdvance, timer') = (timer + fromIntegral advance) `divMod` timerModulus (tac .&. 3)
    writeUnboxedRef timerCounter timer'

    -- Update required
    when (timerAdvance > 0) $ do
      tima <- directReadPort portTIMA
      let tima' = fromIntegral tima + timerAdvance
      if tima' < 0xFF
        then directWritePort portTIMA (fromIntegral tima')
        else do
          -- Handle overflow by loading from TMA
          raiseInterrupt portIF InterruptTimerOverflow
          tma <- directReadPort portTMA
          if tma == 0xFF
            then directWritePort portTIMA 0xFF
            else directWritePort portTIMA
                                 (tma + ((fromIntegral timerAdvance - 1) `mod` (0xFF - tma)))
