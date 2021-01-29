module Machine.GBC.CPU.Interrupts
  ( Interrupt (..),
    flag,
    raise,
    clear,
    getPending,
    getNext,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Word (Word8)
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port

data Interrupt
  = VBlank
  | LCDCStat
  | TimerOverflow
  | EndSerialTransfer
  | P1Low
  | Cancelled
  deriving (Eq, Ord, Show, Bounded, Enum)

flag :: Interrupt -> Word8
flag VBlank = 0x01
flag LCDCStat = 0x02
flag TimerOverflow = 0x04
flag EndSerialTransfer = 0x08
flag P1Low = 0x10
flag Cancelled = 0

{-# INLINE raise #-}
raise :: MonadIO m => Port -> Interrupt -> m ()
raise portIF interrupt = do
  rif <- Port.readDirect portIF
  Port.writeDirect portIF (rif .|. flag interrupt)

{-# INLINE clear #-}
clear :: MonadIO m => Port -> Interrupt -> m ()
clear portIF interrupt = do
  rif <- Port.readDirect portIF
  Port.writeDirect portIF (rif .&. complement (flag interrupt))

-- | Get all of the pending interrupts that are ready to service.
{-# INLINE getPending #-}
getPending :: MonadIO m => Port -> Port -> m Word8
getPending portIF portIE = do
  interrupt <- Port.readDirect portIF
  enabled <- Port.readDirect portIE
  pure (interrupt .&. enabled .&. 0x1F)

-- | Get the next interrupt to service.
{-# INLINE getNext #-}
getNext :: Word8 -> Interrupt
getNext pendingInterrupts = case countTrailingZeros pendingInterrupts of
  0 -> VBlank
  1 -> LCDCStat
  2 -> TimerOverflow
  3 -> EndSerialTransfer
  4 -> P1Low
  _ -> Cancelled
