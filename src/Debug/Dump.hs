module Debug.Dump where

import           GBC.Memory
import           Data.Word
import           Common
import           Control.Monad
import           GBC.Decode
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

dumpDisassembly :: Memory -> Word16 -> IO ()
dumpDisassembly mem base = do
  instructions <- decodeN mem base 10
  forM_ instructions $ \(addr, instruction) ->
    putStrLn $ formatHex addr ++ ": " ++ format instruction

dumpMem :: Memory -> Word16 -> IO ()
dumpMem mem base = forM_ [0 .. 15] $ \line ->
  let offset = base + line * 16 in hexDump offset =<< readChunk mem offset 16

hexDump :: Word16 -> B.ByteString -> IO ()
hexDump base lineData = do
  putStr $ formatHex base
  putStr ": "
  putStr $ unwords $ formatHex <$> B.unpack lineData
  putStr " "
  putStrLn $ toPrintable <$> BC.unpack lineData

toPrintable :: Char -> Char
toPrintable c = if c <= ' ' || c >= '\DEL' then '.' else c
