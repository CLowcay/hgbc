{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Serial
  ( Sync(..)
  , State
  , init
  , ports
  , newSync
  , update
  , notifyIncoming
  )
where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Bits
import           Data.Functor
import           Data.IORef
import           Data.Word
import           Machine.GBC.CPU.Interrupts
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Registers
import           Machine.GBC.Util
import           Prelude                 hiding ( init )

data Sync = Sync {
    out :: MVar Word8
  , inp :: MVar Word8
}

data State = State {
    portSB            :: !(Port Word8)
  , portSC            :: !(Port Word8)
  , portIF            :: !(Port Word8)
  , sync              :: !Sync
  , transferActiveRef :: !(IORef Bool)
  , bitCounter        :: !(UnboxedRef Word8)
  , incoming          :: !(UnboxedRef Word8)
  , clockPeriod       :: !(UnboxedRef Int)
  , shiftClock        :: !Counter
}

flagTransferStart, flagShiftSpeed, flagInternalClock :: Int
flagTransferStart = 7
flagShiftSpeed = 1
flagInternalClock = 0

-- | Create a new serial sync object.
newSync :: IO Sync
newSync = do
  inp <- newEmptyMVar
  out <- newEmptyMVar
  pure Sync { .. }

init :: Sync -> Port Word8 -> IO State
init sync portIF = do
  shiftClock        <- newCounter 0
  bitCounter        <- newUnboxedRef 0
  incoming          <- newUnboxedRef 0xFF
  clockPeriod       <- newUnboxedRef 0
  transferActiveRef <- newIORef False

  portSB            <- newPort 0xFF 0xFF alwaysUpdate
  portSC            <- newPort 0x7C 0x83 $ \_ sc' -> sc' <$ do
    when (sc' `testBit` flagInternalClock) $ do
      if sc' `testBit` flagTransferStart
        then do
          putMVar (out sync) =<< directReadPort portSB
          writeIORef transferActiveRef True
        else writeIORef transferActiveRef False
      writeUnboxedRef clockPeriod (if sc' `testBit` flagShiftSpeed then 4 else 128)
  pure State { .. }

ports :: State -> [(Word16, Port Word8)]
ports State {..} = [(SB, portSB), (SC, portSC)]

-- | Notify an incoming passive transfer
notifyIncoming :: State -> Int -> Word8 -> IO ()
notifyIncoming State {..} period incomingValue = do
  sc <- directReadPort portSC
  unless (sc `testBit` flagInternalClock) $ do
    putMVar (inp sync) incomingValue
    writeUnboxedRef clockPeriod period
    writeIORef transferActiveRef True

-- | Advance the serial clock.
update :: Int -> State -> IO ()
update cycles State {..} = do
  transferActive <- readIORef transferActiveRef
  when transferActive $ do
    reloads <- updateReloadingCounter shiftClock cycles $ readUnboxedRef clockPeriod
    when (reloads > 0) $ do
      counter <- readUnboxedRef bitCounter
      writeUnboxedRef bitCounter =<< clockSerial counter reloads

 where
  clockSerial counter 0      = pure counter
  clockSerial counter clocks = do
    value <- if counter /= 0
      then readUnboxedRef incoming
      else do
        v <- takeMVar (inp sync)
        writeUnboxedRef incoming v
        pure v

    sb <- directReadPort portSB
    let value' = rotateL value 1
    writeUnboxedRef incoming value'
    directWritePort portSB (sb .<<. 1 .|. (value' .&. 1))

    let counter' = (counter + 1) .&. 3
    if counter' /= 0
      then clockSerial counter' (clocks - 1)
      else do
        sc <- directReadPort portSC
        directWritePort portSC (sc .&. 0x7F)
        raiseInterrupt portIF InterruptEndSerialTransfer
        writeIORef transferActiveRef False
        pure counter'
