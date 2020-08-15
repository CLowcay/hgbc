-- | Utility functions.
module Machine.GBC.Util
  ( (.<<.),
    (.>>.),
    isFlagSet,
    formatHex,
  )
where

import Data.Bits
import Data.Word

infixl 8 .<<.

{-# INLINE (.<<.) #-}
(.<<.) :: Bits a => a -> Int -> a
a .<<. b = unsafeShiftL a b

infixl 8 .>>.

{-# INLINE (.>>.) #-}
(.>>.) :: Bits a => a -> Int -> a
a .>>. b = unsafeShiftR a b

{-# INLINE isFlagSet #-}
isFlagSet :: Word8 -> Word8 -> Bool
isFlagSet flag v = v .&. flag /= 0

padLeft :: Int -> Char -> String -> String
padLeft width c s = reverse . take width $ reverse s ++ repeat c

{-# INLINEABLE formatHex #-}
formatHex :: (Show b, Num b, FiniteBits b) => b -> String
formatHex n = padLeft width '0' $ encodeHex . toHexit <$> reverse [0 .. lastHexit]
  where
    width = finiteBitSize n `div` 4
    lastHexit = (finiteBitSize n `div` 4) - 1
    toHexit i = (n .>>. (i * 4)) .&. 0x0F
    encodeHex 0 = '0'
    encodeHex 1 = '1'
    encodeHex 2 = '2'
    encodeHex 3 = '3'
    encodeHex 4 = '4'
    encodeHex 5 = '5'
    encodeHex 6 = '6'
    encodeHex 7 = '7'
    encodeHex 8 = '8'
    encodeHex 9 = '9'
    encodeHex 10 = 'A'
    encodeHex 11 = 'B'
    encodeHex 12 = 'C'
    encodeHex 13 = 'D'
    encodeHex 14 = 'E'
    encodeHex 15 = 'F'
    encodeHex x = error $ "Impossible hexit " ++ show x
