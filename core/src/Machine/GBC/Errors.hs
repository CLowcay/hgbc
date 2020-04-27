module Machine.GBC.Errors
  ( Fault(..)
  )
where

import           Control.Exception
import           Data.Word
import           Machine.GBC.Util

-- | A fault in the emulated hardware. On the real hardware these conditions
-- lead to undefined behavior.
data Fault = InvalidInstruction Word8    -- ^ Attempted to execute an invalid instruction.
           | InvalidWrite Word16         -- ^ Wrote to an invalid write address.
           | InvalidRead Word16          -- ^ Read from an invalid address.
           | InvalidAccess Word16        -- ^ Invalid access on cartridge RAM.
           | InvalidSourceForDMA Word16  -- ^ DMA from an invalid source address.
           deriving (Eq, Ord, Show)

instance Exception Fault where
  displayException (InvalidInstruction  w8  ) = "Invalid instruction " ++ formatHex w8
  displayException (InvalidWrite  addr) = "Write to invalid RAM address " ++ formatHex addr
  displayException (InvalidRead addr) = "Read from invalid RAM address " ++ formatHex addr
  displayException (InvalidAccess addr) = "Read or Write at invalid RAM address " ++ formatHex addr
  displayException (InvalidSourceForDMA addr) = "Invalid source address for DMG DMA" ++ formatHex addr
