{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Machine.GBC.Timer
  ( State
  , init
  , ports
  , update
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
import           Prelude                 hiding ( init )

-- | The TIMA register behaves differently for 2 cycles after it overflows, so
-- we need to keep track of these states.
data TIMAState
  = TIMANormal
  | TIMAOverflow     -- ^ TIMA overflowed on the last cycle
  | TIMAReload       -- ^ TIMA is reloading from TMA
  deriving (Eq, Ord, Show)

-- | The current state of the timer hardware.
data State = State {
    systemDIV    :: !(UnboxedRef Word16)
  , edgeMaskRef  :: !(UnboxedRef Word16)
  , lastEdgeRef  :: !(UnboxedRef Word16)
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
init :: IO () -> Port Word8 -> Port Word8 -> IO State
init clockAudio portKEY1 portIF = do
  systemDIV    <- newUnboxedRef 0
  edgeMaskRef  <- newUnboxedRef (decodeTimaMask 0)
  lastEdgeRef  <- newUnboxedRef 0
  timaStateRef <- newIORef TIMANormal
  portDIV      <- newPortWithReadAction
    0x00
    0x00
    (\_ -> do
      c <- readUnboxedRef systemDIV
      pure (fromIntegral (c .>>. 8))
    )
    (\v _ -> v <$ writeUnboxedRef systemDIV 1)
  portTIMA <- newPort 0x00 0xFF $ \_ v' -> v' <$ do
    timaState <- readIORef timaStateRef
    when (timaState == TIMAOverflow) $ writeIORef timaStateRef TIMANormal
  portTMA <- newPort 0x00 0xFF $ \_ v' -> v' <$ do
    timaState <- readIORef timaStateRef
    when (timaState == TIMAReload) $ directWritePort portTIMA v'
  portTAC <- newPort 0xF8 0x07 $ \_ v' -> v' <$ do
    edgeMask <- readUnboxedRef edgeMaskRef
    writeUnboxedRef edgeMaskRef ((edgeMask .&. complement allTimaBits) .|. decodeTimaMask v')
  pure State { .. }

ports :: State -> [(Word16, Port Word8)]
ports State {..} = [(DIV, portDIV), (TIMA, portTIMA), (TMA, portTMA), (TAC, portTAC)]

allTimaBits :: Word16
allTimaBits = 0x02A8

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
fallingEdge mask v v' = mask .&. v /= 0 && mask .&. v' == 0

-- | Update the systemDIV, IF, TIMA, and TIMA state.
updateClocks
  :: Word16                            -- ^ TIMA edge detector bit
  -> Word8                             -- ^ TMA register
  -> Word16                            -- ^ TIMA enabled
  -> Word8                             -- ^ Initial IF register
  -> Word8                             -- ^ Initial TIMA register
  -> TIMAState                         -- ^ Initial TIMA state
  -> Word16                            -- ^ Previous edge detector input
  -> Word16                            -- ^ Initial systemDIV
  -> Int                               -- ^ Clocks to advance
  -> (Word16, Word16, Word8, Word8, TIMAState) -- ^ (systemDIV, edge, IF, TIMA, TIMA state)
updateClocks edgeMask tma timaEnabled = outerLoop
 where
  outerLoop !rif !tima !timaState = innerLoop
   where
    innerLoop !systemClock !edge 0 = (systemClock, edge, rif, tima, timaState)
    innerLoop !systemClock !edge !cycles =
      let systemClock' = systemClock + 4
          cycles'      = cycles - 1
          edge'        = edgeMask .&. systemClock' .&. timaEnabled
      in  case timaState of
            TIMANormal -> if fallingEdge allTimaBits edge edge'
              then if tima == 0xFF
                then outerLoop rif 0 TIMAOverflow systemClock' edge' cycles'
                else outerLoop rif (tima + 1) TIMANormal systemClock' edge' cycles'
              else innerLoop systemClock' edge' cycles'
            TIMAOverflow ->
              outerLoop (rif .|. timerInterrupt) tma TIMAReload systemClock' edge' cycles'
            TIMAReload -> outerLoop rif tma TIMANormal systemClock' edge' cycles'

allAudioFrameBits :: Word16
allAudioFrameBits = 0x3000

update :: State -> Int -> IO ()
update State {..} advance = do
  key1 <- directReadPort portKEY1
  let doubleSpeed    = fromIntegral (key1 .>>. 7)
  let audioFrameMask = 0x1000 .<<. doubleSpeed
  tima <- directReadPort portTIMA
  tma  <- directReadPort portTMA
  tac  <- directReadPort portTAC
  rif  <- directReadPort portIF
  let timaEnabled = if timerStarted tac then 0xFFFF else 0
  systemDIV0 <- readUnboxedRef systemDIV
  timaMask   <- readUnboxedRef edgeMaskRef
  timaState  <- readIORef timaStateRef
  lastEdge   <- readUnboxedRef lastEdgeRef
  let (systemDIV1, edge, rif', tima', timaState') =
        updateClocks timaMask tma timaEnabled rif tima timaState systemDIV0 lastEdge advance
  let edge' = edge .|. (audioFrameMask .&. systemDIV1)
  writeUnboxedRef lastEdgeRef edge'
  directWritePort portTIMA tima'
  directWritePort portIF   rif'
  writeIORef timaStateRef $! timaState'
  writeUnboxedRef systemDIV systemDIV1
  when (fallingEdge allAudioFrameBits lastEdge edge') clockAudio
