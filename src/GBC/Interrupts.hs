module GBC.Interrupts
  ( Interrupt(..)
  , flagInterrupt
  , raiseInterrupt
  , clearInterrupt
  , pendingEnabledInterrupts
  , getNextInterrupt
  )
where

import           Data.Word
import           Data.Bits
import           GBC.Primitive

data Interrupt = InterruptVBlank
               | InterruptLCDCStat
               | InterruptTimerOverflow
               | InterruptEndSerialTransfer
               | InterruptP1Low
               deriving (Eq, Ord, Show, Bounded, Enum)

flagInterrupt :: Interrupt -> Word8
flagInterrupt InterruptVBlank            = 0x01
flagInterrupt InterruptLCDCStat          = 0x02
flagInterrupt InterruptTimerOverflow     = 0x04
flagInterrupt InterruptEndSerialTransfer = 0x08
flagInterrupt InterruptP1Low             = 0x10

{-# INLINE raiseInterrupt #-}
raiseInterrupt :: Port Word8 -> Interrupt -> IO ()
raiseInterrupt portIF interrupt = setPortBits portIF (flagInterrupt interrupt)

{-# INLINE clearInterrupt #-}
clearInterrupt :: Port Word8 -> Interrupt -> IO ()
clearInterrupt portIF interrupt = clearPortBits portIF (flagInterrupt interrupt)

-- | Get all of the pending interrupts that are ready to service.
{-# INLINE pendingEnabledInterrupts #-}
pendingEnabledInterrupts :: Port Word8 -> Port Word8 -> IO Word8
pendingEnabledInterrupts portIF portIE = do
  interrupt <- readPort portIF
  enabled   <- readPort portIE
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
  n -> error ("Invalid pending interrupt " ++ show n)