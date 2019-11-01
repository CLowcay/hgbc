module GBC.Errors
  ( Fault(..)
  )
where

import           Control.Exception
import           Data.Word
import           Common

data Fault = InvalidInstruction Word8
           | InvalidWrite Word16
           | InvalidRead Word16
           | InvalidAccess Word16
           | InvalidSourceForDMA Word16
           deriving (Eq, Ord, Show)

instance Exception Fault where
  displayException (InvalidInstruction  w8  ) = "Invalid instruction " ++ formatHex w8
  displayException (InvalidWrite  addr) = "Write to invalid RAM address " ++ formatHex addr
  displayException (InvalidRead addr) = "Read from invalid RAM address " ++ formatHex addr
  displayException (InvalidAccess addr) = "Read or Write at invalid RAM address " ++ formatHex addr
  displayException (InvalidSourceForDMA addr) = "Invalid source address for DMG DMA" ++ formatHex addr
