module Machine.GBC.Errors
  ( Fault (..),
  )
where

import Control.Exception (Exception (displayException))
import Data.Word (Word16, Word8)
import Machine.GBC.Util (formatHex)

-- | A fault in the emulated hardware. On the real hardware these conditions
-- lead to undefined behavior.
data Fault
  = -- | Attempted to execute an invalid instruction.
    InvalidInstruction Word8
  | -- | Wrote to an invalid write address.
    InvalidWrite Word16
  | -- | Read from an invalid address.
    InvalidRead Word16
  | -- | Invalid access on cartridge RAM.
    InvalidAccess Word16
  | -- | DMA from an invalid source address.
    InvalidSourceForDMA Word16
  deriving (Eq, Ord, Show)

instance Exception Fault where
  displayException (InvalidInstruction w8) = "Invalid instruction " ++ formatHex w8
  displayException (InvalidWrite addr) = "Write to invalid RAM address " ++ formatHex addr
  displayException (InvalidRead addr) = "Read from invalid RAM address " ++ formatHex addr
  displayException (InvalidAccess addr) = "Read or Write at invalid RAM address " ++ formatHex addr
  displayException (InvalidSourceForDMA addr) = "Invalid source address for DMG DMA" ++ formatHex addr
