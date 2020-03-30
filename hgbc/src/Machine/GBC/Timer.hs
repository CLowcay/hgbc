{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
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
import           Data.IORef
import           Data.Word
import           Machine.GBC.CPU.Interrupts
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Registers
import           Machine.GBC.Util

-- | The TIMA register behaves differently for 2 cycles after it overflows, so
-- we need to keep track of these states.
data TIMAState
  = TIMANormal
  | TIMAOverflow     -- ^ TIMA overflowed on the last cycle
  | TIMAReload       -- ^ TIMA is reloading from TMA
  deriving (Eq, Ord, Show)

-- | The current state of the timer hardware.
data TimerState = TimerState {
    systemDIV    :: !(UnboxedRef Word16)
  , resetDIVRef  :: !(IORef Bool)
  , extraClockRef:: !(IORef Bool)
  , timaMaskRef  :: !(UnboxedRef Word16)
  , timaStateRef :: !(IORef TIMAState)
  , clockAudio   :: !(IO ())
  , portDIV      :: !(Port Word8)
  , portTIMA     :: !(Port Word8)
  , portTMA      :: !(Port Word8)
  , portTAC      :: !(Port Word8)
  , portKEY1     :: !(Port Word8)
  , portIF       :: !(Port Word8)
}

-- | Create the initial timer state.
initTimerState :: IO () -> Port Word8 -> Port Word8 -> IO TimerState
initTimerState clockAudio portKEY1 portIF = do
  systemDIV     <- newUnboxedRef 0
  resetDIVRef   <- newIORef False
  extraClockRef <- newIORef False
  timaMaskRef   <- newUnboxedRef (decodeTimaMask 0)
  timaStateRef  <- newIORef TIMANormal
  portDIV       <- newPortWithReadAction
    0x00
    0x00
    (\_ -> do
      c <- readUnboxedRef systemDIV
      pure (fromIntegral (c .>>. 8))
    )
    (\v _ -> v <$ writeIORef resetDIVRef True)
  portTIMA <- newPort 0x00 0xFF $ \_ v' -> v' <$ do
    timaState <- readIORef timaStateRef
    when (timaState == TIMAOverflow) $ writeIORef timaStateRef TIMANormal
  portTMA <- newPort 0x00 0xFF $ \_ v' -> v' <$ do
    timaState <- readIORef timaStateRef
    when (timaState == TIMAReload) $ directWritePort portTIMA v'
  portTAC <- newPort 0x00 0x07 $ \v v' -> v' <$ do
    systemDIV0 <- readUnboxedRef systemDIV
    timaMask   <- readUnboxedRef timaMaskRef
    let timaMask'    = decodeTimaMask v'
    let timerSwitch0 = systemDIV0 .&. timaMask /= 0 && timerStarted v
    let timerSwitch1 = systemDIV0 .&. timaMask' /= 0 && timerStarted v'
    when (timerSwitch0 && not timerSwitch1) $ writeIORef extraClockRef True
    writeUnboxedRef timaMaskRef timaMask'
  pure TimerState { .. }

timerPorts :: TimerState -> [(Word16, Port Word8)]
timerPorts TimerState {..} = [(DIV, portDIV), (TIMA, portTIMA), (TMA, portTMA), (TAC, portTAC)]

decodeTimaMask :: Word8 -> Word16
decodeTimaMask tac = case tac .&. 3 of
  0 -> 0x0200
  1 -> 0x0008
  2 -> 0x0020
  3 -> 0x0080
  x -> error ("Invalid timer modulus " <> show x)

-- | Check the timer stop flag for the TAC register.
timerStarted :: Word8 -> Bool
timerStarted = (`testBit` 2)

timerInterrupt :: Word8
timerInterrupt = flagInterrupt InterruptTimerOverflow

-- | Detect a falling edge.
fallingEdge :: (Bits a, Integral a) => a -> a -> a -> Bool
fallingEdge mask v v' = mask .&. v .&. complement (mask .&. v') /= 0

-- | Update the systemDIV, IF, TIMA, and TIMA state.
updateClocks
  :: Word16                            -- ^ TIMA edge detector bit
  -> Word8                             -- ^ TMA register
  -> Bool                              -- ^ TIMA enabled
  -> Word8                             -- ^ Initial IF register
  -> Word8                             -- ^ Initial TIMA register
  -> TIMAState                         -- ^ Initial TIMA state
  -> Bool                              -- ^ Reset systemDIV on the next cycle
  -> Bool                              -- ^ True if TIMA needs an extra clock
  -> Word16                            -- ^ Initial systemDIV
  -> Int                               -- ^ Clocks to advance
  -> (Word16, Word8, Word8, TIMAState) -- ^ (systemDIV, IF, TIMA, TIMA state)
updateClocks timaMask tma timaEnabled = outerLoop
 where
  outerLoop !rif !tima !timaState !reset !extraClock = innerLoop
   where
    innerLoop !systemClock 0 = (systemClock, rif, tima, timaState)
    innerLoop !systemClock !cycles =
      let systemClock' = if reset then 1 else systemClock + 4
          cycles'      = cycles - 1
      in  case timaState of
            TIMANormal ->
              if extraClock || (timaEnabled && fallingEdge timaMask systemClock systemClock')
                then if tima == 0xFF
                  then outerLoop rif 0 TIMAOverflow False False systemClock' cycles'
                  else outerLoop rif (tima + 1) TIMANormal False False systemClock' cycles'
                else innerLoop systemClock' cycles'
            TIMAOverflow ->
              outerLoop (rif .|. timerInterrupt) tma TIMAReload False False systemClock' cycles'
            TIMAReload -> outerLoop rif tma TIMANormal False False systemClock' cycles'

updateTimer :: TimerState -> Int -> IO ()
updateTimer TimerState {..} advance = do
  key1 <- directReadPort portKEY1
  let doubleSpeed    = fromIntegral (key1 .>>. 7)
  let audioFrameMask = 0x1000 .<<. doubleSpeed
  tima <- directReadPort portTIMA
  tma  <- directReadPort portTMA
  tac  <- directReadPort portTAC
  rif  <- directReadPort portIF
  let timaEnabled = timerStarted tac
  systemDIV0 <- readUnboxedRef systemDIV
  timaMask   <- readUnboxedRef timaMaskRef
  timaState  <- readIORef timaStateRef
  resetDIV   <- readIORef resetDIVRef
  extraClock <- readIORef extraClockRef
  let (systemDIV1, rif', tima', timaState') = updateClocks timaMask
                                                           tma
                                                           timaEnabled
                                                           rif
                                                           tima
                                                           timaState
                                                           resetDIV
                                                           extraClock
                                                           systemDIV0
                                                           advance
  directWritePort portTIMA tima'
  directWritePort portIF   rif'
  writeIORef timaStateRef $! timaState'
  writeUnboxedRef systemDIV systemDIV1
  when resetDIV $ writeIORef resetDIVRef False
  when extraClock $ writeIORef extraClockRef False

  when (fallingEdge audioFrameMask systemDIV0 systemDIV1) clockAudio

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
    let modulus = case tac .&. 3 of
          0 -> 1024
          1 -> 16
          2 -> 64
          3 -> 256
          x -> error ("Invalid timer modulus " <> show x)
        frequency = (4 * 1024 * 1024 :: Int) `div` modulus
    in  [("Frequency", show frequency), ("Timer Started", show (timerStarted tac) ++ "Hz")]
