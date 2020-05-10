{-# LANGUAGE RecordWildCards #-}
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
timerStarted tac = tac `testBit` 2

-- | Detect a falling edge.
fallingEdge :: Word16 -> Word16 -> Word16 -> Bool
fallingEdge mask v v' = mask .&. v /= 0 && mask .&. v' == 0

allAudioFrameBits :: Word16
allAudioFrameBits = 0x3000

update :: State -> IO ()
update State {..} = do
  key1 <- directReadPort portKEY1
  let doubleSpeed    = fromIntegral (key1 .>>. 7)
  let audioFrameMask = 0x1000 .<<. doubleSpeed

  systemDIV0 <- readUnboxedRef systemDIV
  let systemDIV1 = systemDIV0 + 4
  writeUnboxedRef systemDIV systemDIV1

  tac      <- directReadPort portTAC
  edgeMask <- readUnboxedRef edgeMaskRef
  let edge' = if timerStarted tac then edgeMask .&. systemDIV1 else 0

  edge      <- readUnboxedRef lastEdgeRef
  timaState <- readIORef timaStateRef
  case timaState of
    TIMANormal -> when (fallingEdge allTimaBits edge edge') $ do
      tima <- directReadPort portTIMA
      if tima == 0xFF
        then do
          writeIORef timaStateRef TIMAOverflow
          directWritePort portTIMA 0
        else directWritePort portTIMA (tima + 1)
    TIMAOverflow -> do
      raiseInterrupt portIF InterruptTimerOverflow
      writeIORef timaStateRef TIMAReload
      directWritePort portTIMA =<< directReadPort portTMA
    TIMAReload -> do
      writeIORef timaStateRef TIMANormal
      directWritePort portTIMA =<< directReadPort portTMA

  let edge'' = edge' .|. (audioFrameMask .&. systemDIV1)
  writeUnboxedRef lastEdgeRef edge''
  when (fallingEdge allAudioFrameBits edge edge'') clockAudio
