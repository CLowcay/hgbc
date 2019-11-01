module GBC.Errors
  ( Fault(..)
  )
where

import           Control.Exception
import           Data.Word
import           Common

data Fault = InvalidInstruction Word8
           | WriteToDisabledRAM Word16
           | ReadFromDisabledRAM Word16
           deriving (Eq, Ord, Show)

instance Exception Fault where
  displayException (InvalidInstruction  w8  ) = "Invalid instruction " ++ formatHex w8
  displayException (WriteToDisabledRAM  addr) = "Write to invalid RAM address " ++ formatHex addr
  displayException (ReadFromDisabledRAM addr) = "Read from invalid RAM address " ++ formatHex addr
