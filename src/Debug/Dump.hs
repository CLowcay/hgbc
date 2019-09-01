{-# LANGUAGE RecordWildCards #-}
module Debug.Dump where

import           GBC.Memory
import           GBC.CPU
import           GBC.ROM
import           Data.Word
import           Common
import           Control.Monad
import           GBC.Decode
import           Data.Bits
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

dumpRegisters :: RegisterFile -> IO ()
dumpRegisters RegisterFile {..} = do
  putStrLn $ "F=" ++ formatHex regF ++ " A=" ++ formatHex regA ++ " AF=" ++ formatHex
    (assemble regA regF)
  putStrLn $ "B=" ++ formatHex regB ++ " C=" ++ formatHex regC ++ " BC=" ++ formatHex
    (assemble regB regC)
  putStrLn $ "D=" ++ formatHex regD ++ " E=" ++ formatHex regE ++ " DE=" ++ formatHex
    (assemble regD regE)
  putStrLn $ "H=" ++ formatHex regH ++ " L=" ++ formatHex regL ++ " HL=" ++ formatHex
    (assemble regH regL)
  putStrLn $ "PC=" ++ formatHex regPC ++ "   SP=" ++ formatHex regSP
  putStrLn
    $  "flags="
    ++ [ if regF .&. flagZ /= 0 then 'Z' else 'z'
       , if regF .&. flagN /= 0 then 'N' else 'n'
       , if regF .&. flagH /= 0 then 'H' else 'h'
       , if regF .&. flagCY /= 0 then 'C' else 'c'
       , if regHidden .&. flagIME /= 0 then 'I' else 'i'
       ]
  where assemble h l = (fromIntegral h `unsafeShiftL` 8 .|. fromIntegral l) :: Word16

dumpHeader :: Header -> IO ()
dumpHeader Header {..} = do
  putStrLn
    $  take 11 (BC.unpack gameTitle ++ repeat ' ')
    ++ BC.unpack gameCode
    ++ " "
    ++ case destination of
         Japan    -> " (JAPAN)"
         Overseas -> " (INTERNATIONAL)"

  putStrLn $ "Version: " ++ show maskROMVersion

  putStrLn $ "Maker: " ++ formatHex oldLicenseCode ++ " " ++ BC.unpack makerCode

  putStr $ "Console support: " ++ case cgbSupport of
    CGBIncompatible -> "GB"
    CGBCompatible   -> "GB+CGB"
    CGBExclusive    -> "CGB"
  putStrLn $ case sgbSupport of
    GBOnly  -> ""
    UsesSGB -> "+SGB"

  putStr $ "Cartridge: " ++ case mbcType cartridgeType of
    Nothing      -> "No MBC"
    Just MBC1    -> "MBC1"
    Just MBC2    -> "MBC2"
    Just MBC3    -> "MBC3"
    Just MBC3RTC -> "MBC3+RTC"
    Just MBC5    -> "MBC5"
  when (hasSRAM cartridgeType) $ putStr "+SRAM"
  when (hasBackupBattery cartridgeType) $ putStr "+Battery"
  putStr $ " (" ++ formatByteCount romSize ++ " ROM)"
  when (externalRAM > 0) $ putStr $ " + " ++ formatByteCount externalRAM ++ " RAM"
  putStrLn ""

  putStrLn $ "Start address: " ++ formatHex startAddress

formatByteCount :: Int -> String
formatByteCount b | b < 1024        = show b
                  | b < 1024 * 1024 = show (b `div` 1024) ++ "KiB"
                  | otherwise       = show (b `div` (1024 * 1024)) ++ "MiB"

dumpDisassembly :: (Word16 -> IO String) -> Memory -> Word16 -> Int -> IO ()
dumpDisassembly decorator mem base n = do
  instructions <- decodeN mem base n
  forM_ instructions $ \(addr, instruction) -> do
    decoration <- decorator addr
    putStrLn $ decoration ++ formatHex addr ++ ": " ++ format instruction

dumpMem :: Memory -> Word16 -> IO ()
dumpMem mem base = forM_ [0 .. 15]
  $ \line -> let offset = base + line * 16 in hexDump offset =<< readChunk mem offset 16

hexDump :: Word16 -> B.ByteString -> IO ()
hexDump base lineData = do
  putStr $ formatHex base
  putStr ": "
  putStr $ unwords $ formatHex <$> B.unpack lineData
  putStr " "
  putStrLn $ toPrintable <$> BC.unpack lineData

toPrintable :: Char -> Char
toPrintable c = if c <= ' ' || c >= '\DEL' then '.' else c
