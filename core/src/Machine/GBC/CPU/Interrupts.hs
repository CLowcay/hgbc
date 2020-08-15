module Machine.GBC.CPU.Interrupts
  ( Interrupt (..),
    flagInterrupt,
    raiseInterrupt,
    clearInterrupt,
    pendingEnabledInterrupts,
    getNextInterrupt,
  )
where

import Control.Monad.IO.Class
import Data.Bits
import Data.Word
import Machine.GBC.Primitive

data Interrupt
  = InterruptVBlank
  | InterruptLCDCStat
  | InterruptTimerOverflow
  | InterruptEndSerialTransfer
  | InterruptP1Low
  | InterruptCancelled
  deriving (Eq, Ord, Show, Bounded, Enum)

flagInterrupt :: Interrupt -> Word8
flagInterrupt InterruptVBlank = 0x01
flagInterrupt InterruptLCDCStat = 0x02
flagInterrupt InterruptTimerOverflow = 0x04
flagInterrupt InterruptEndSerialTransfer = 0x08
flagInterrupt InterruptP1Low = 0x10
flagInterrupt InterruptCancelled = 0

{-# INLINE raiseInterrupt #-}
raiseInterrupt :: MonadIO m => Port -> Interrupt -> m ()
raiseInterrupt portIF interrupt = do
  rif <- directReadPort portIF
  directWritePort portIF (rif .|. flagInterrupt interrupt)

{-# INLINE clearInterrupt #-}
clearInterrupt :: MonadIO m => Port -> Interrupt -> m ()
clearInterrupt portIF interrupt = do
  rif <- directReadPort portIF
  directWritePort portIF (rif .&. complement (flagInterrupt interrupt))

-- | Get all of the pending interrupts that are ready to service.
{-# INLINE pendingEnabledInterrupts #-}
pendingEnabledInterrupts :: MonadIO m => Port -> Port -> m Word8
pendingEnabledInterrupts portIF portIE = do
  interrupt <- directReadPort portIF
  enabled <- directReadPort portIE
  pure (interrupt .&. enabled .&. 0x1F)

-- | Get the next interrupt to service.
{-# INLINE getNextInterrupt #-}
getNextInterrupt :: Word8 -> Interrupt
getNextInterrupt pendingInterrupts = case countTrailingZeros pendingInterrupts of
  0 -> InterruptVBlank
  1 -> InterruptLCDCStat
  2 -> InterruptTimerOverflow
  3 -> InterruptEndSerialTransfer
  4 -> InterruptP1Low
  _ -> InterruptCancelled
