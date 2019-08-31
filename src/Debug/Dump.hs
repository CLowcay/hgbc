module Debug.Dump where

import GBC.Memory
import Data.Word
import Data.Bits
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

dumpMem :: Memory -> Word16 -> IO ()
dumpMem mem base =
  forM_ [0..15] $ \line -> 
    let offset = base + line * 16
    in hexDump offset =<< readChunk mem offset 16

hexDump :: Word16 -> B.ByteString -> IO ()
hexDump base lineData = do
    putStr $ formatHex 4 base
    putStr ": "
    putStr $ unwords $ formatHex 2 <$> B.unpack lineData
    putStr " "
    putStrLn $ toPrintable <$> BC.unpack lineData

toPrintable :: Char -> Char
toPrintable c = if c <= ' ' || c >= '\DEL' then '.' else c

padLeft :: Int -> String -> String
padLeft width s = reverse . take width $ reverse s ++ repeat '0'

formatHex :: (Show b, Num b, FiniteBits b) => Int -> b -> String
formatHex width n = padLeft width $ encodeHex . toHexit <$> reverse [0 .. lastHexit]
    where
      lastHexit = (finiteBitSize n `div` 4) - 1
      toHexit i = (n `unsafeShiftR` (i * 4)) .&. 0x0F
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
