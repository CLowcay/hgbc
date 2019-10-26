{-# LANGUAGE RecordWildCards #-}
module Debug.Dump
  ( dumpRegisters
  , dumpHeader
  , dumpDisassembly
  , dumpMem
  )
where

import           Common
import           Control.Monad
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.Word
import           GBC.CPU
import           GBC.Decode
import           GBC.ISA
import           GBC.Memory
import           GBC.ROM
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

dumpDisassembly
  :: HasMemory env => (Word16 -> IO String) -> SymbolTable -> Word16 -> Int -> ReaderT env IO ()
dumpDisassembly decorator symbolTable base n = do
  instructions <- decodeN base n
  for_ instructions $ \(addr, instruction) -> do
    decoration <- liftIO $ decorator addr
    case lookupByAddress symbolTable addr of
      Nothing    -> pure ()
      Just label -> liftIO $ putStrLn $ (' ' <$ decoration) ++ label ++ ":"
    liftIO
      $  putStrLn
      $  decoration
      ++ formatHex addr
      ++ ": "
      ++ formatWithSymbolTable symbolTable instruction
      ++ extraInfo addr instruction
 where
  extraInfo addr (JR e) = " [" ++ formatOrLookup16 symbolTable (addr + 2 + fromIntegral e) ++ "]"
  extraInfo addr (JRCC _ e) = " [" ++ formatOrLookup16 symbolTable (addr + 2 + fromIntegral e) ++ "]"
  extraInfo _    _        = ""

dumpMem :: HasMemory env => Word16 -> ReaderT env IO ()
dumpMem base = for_ [0 .. 15]
  $ \line -> let offset = base + line * 16 in liftIO . hexDump offset =<< readChunk offset 16

hexDump :: Word16 -> B.ByteString -> IO ()
hexDump base lineData = do
  putStr $ formatHex base
  putStr ": "
  putStr $ unwords $ formatHex <$> B.unpack lineData
  putStr " "
  putStrLn $ toPrintable <$> BC.unpack lineData

toPrintable :: Char -> Char
toPrintable c = if c <= ' ' || c >= '\DEL' then '.' else c
