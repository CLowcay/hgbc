module Common
  ( isFlagSet
  , SymbolTable(..)
  , lookupByAddress
  , lookupBySymbol
  , Format(..)
  , formatHex
  )
where

import           Data.Bits
import           Data.Word
import qualified Data.HashMap.Strict           as HM

isFlagSet :: Word8 -> Word8 -> Bool
isFlagSet flag v = v .&. flag /= 0

data SymbolTable = SymbolTable !(HM.HashMap Word16 String) !(HM.HashMap String Word16)

lookupByAddress :: SymbolTable -> Word16 -> Maybe String
lookupByAddress (SymbolTable addrTable _) addr = HM.lookup addr addrTable

lookupBySymbol :: SymbolTable -> String -> Maybe Word16
lookupBySymbol (SymbolTable _ symbolTable) label = HM.lookup label symbolTable

class Format c where
  format :: c -> String
  format = formatWithSymbolTable $ SymbolTable HM.empty HM.empty
  formatWithSymbolTable :: SymbolTable -> c -> String
  formatWithSymbolTable _ = format

{-# INLINABLE padLeft #-}
padLeft :: Int -> String -> String
padLeft width s = reverse . take width $ reverse s ++ repeat '0'

{-# INLINABLE formatHex #-}
formatHex :: (Show b, Num b, FiniteBits b) => b -> String
formatHex n = padLeft width $ encodeHex . toHexit <$> reverse [0 .. lastHexit]
 where
  width     = finiteBitSize n `div` 4
  lastHexit = (finiteBitSize n `div` 4) - 1
  toHexit i = (n `unsafeShiftR` (i * 4)) .&. 0x0F
  encodeHex 0  = '0'
  encodeHex 1  = '1'
  encodeHex 2  = '2'
  encodeHex 3  = '3'
  encodeHex 4  = '4'
  encodeHex 5  = '5'
  encodeHex 6  = '6'
  encodeHex 7  = '7'
  encodeHex 8  = '8'
  encodeHex 9  = '9'
  encodeHex 10 = 'A'
  encodeHex 11 = 'B'
  encodeHex 12 = 'C'
  encodeHex 13 = 'D'
  encodeHex 14 = 'E'
  encodeHex 15 = 'F'
  encodeHex x  = error $ "Impossible hexit " ++ show x
