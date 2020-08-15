{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Serial
  ( Sync (..),
    State,
    init,
    ports,
    newSync,
    update,
    notifyIncoming,
  )
where

import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.Functor
import Data.IORef
import Data.Word
import Machine.GBC.CPU.Interrupts
import Machine.GBC.Mode
import Machine.GBC.Primitive
import Machine.GBC.Primitive.UnboxedRef
import Machine.GBC.Registers
import Machine.GBC.Util
import Prelude hiding (init)

data Sync = Sync
  { out :: MVar Word8,
    inp :: MVar Word8
  }

data State = State
  { portSB :: !Port,
    portSC :: !Port,
    portIF :: !Port,
    sync :: !Sync,
    transferActiveRef :: !(IORef Bool),
    bitCounter :: !(UnboxedRef Word8),
    incoming :: !(UnboxedRef Word8),
    clockPeriod :: !(UnboxedRef Int),
    shiftClock :: !Counter
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
  pure Sync {..}

init :: Sync -> Port -> IORef EmulatorMode -> IO State
init sync portIF modeRef = do
  shiftClock <- newCounter 0
  bitCounter <- newUnboxedRef 0
  incoming <- newUnboxedRef 0xFF
  clockPeriod <- newUnboxedRef 0
  transferActiveRef <- newIORef False

  portSB <- newPort 0x00 0xFF alwaysUpdate
  portSC <-
    newPortWithReadAction
      0x7C
      0x83
      ( \sc -> do
          mode <- readIORef modeRef
          pure (if mode == DMG then sc .|. 0x7E else sc)
      )
      ( \_ sc' ->
          sc' <$ do
            when (sc' `testBit` flagInternalClock) $ do
              if sc' `testBit` flagTransferStart
                then do
                  putMVar (out sync) =<< directReadPort portSB
                  writeIORef transferActiveRef True
                else writeIORef transferActiveRef False
              mode <- readIORef modeRef
              writeUnboxedRef clockPeriod (if sc' `testBit` flagShiftSpeed && mode /= DMG then 4 else 128)
      )
  pure State {..}

ports :: State -> [(Word16, Port)]
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
update :: State -> IO ()
update State {..} = do
  transferActive <- readIORef transferActiveRef
  when transferActive $
    updateCounter shiftClock 1 $ do
      counter <- readUnboxedRef bitCounter
      writeUnboxedRef bitCounter =<< clockSerial counter
      readUnboxedRef clockPeriod
  where
    clockSerial counter = do
      value <-
        if counter /= 0
          then readUnboxedRef incoming
          else do
            v <- takeMVar (inp sync)
            v <$ writeUnboxedRef incoming v

      sb <- directReadPort portSB
      let value' = rotateL value 1
      writeUnboxedRef incoming value'
      directWritePort portSB (sb .<<. 1 .|. (value' .&. 1))

      let counter' = (counter + 1) .&. 3
      when (counter' == 0) $ do
        sc <- directReadPort portSC
        directWritePort portSC (sc .&. 0x7F)
        raiseInterrupt portIF InterruptEndSerialTransfer
        writeIORef transferActiveRef False
      pure counter'
