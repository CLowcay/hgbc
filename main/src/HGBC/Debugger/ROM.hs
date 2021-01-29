{-# LANGUAGE RecordWildCards #-}

module HGBC.Debugger.ROM
  ( dumpHeader,
  )
where

import qualified Data.ByteString.Char8 as BC
import qualified Machine.GBC.ROM as ROM
import Machine.GBC.Util (formatHex)

dumpHeader :: ROM.Header -> IO ()
dumpHeader ROM.Header {..} = do
  putStrLn $
    take 16 (BC.unpack gameTitle ++ repeat ' ')
      ++ " "
      ++ BC.unpack gameCode
      ++ " "
      ++ case destination of
        ROM.Japan -> " (JAPAN)"
        ROM.Overseas -> " (INTERNATIONAL)"

  putStrLn ("Version: " ++ show maskROMVersion)
  putStrLn
    ( "Licensee code: "
        ++ if oldLicenseCode == 0x33 then BC.unpack makerCode else formatHex oldLicenseCode <> "h"
    )

  putStr $
    "Console support: " ++ case cgbSupport of
      ROM.CGBIncompatible -> "GB"
      ROM.CGBCompatible -> "GB+CGB"
      ROM.CGBExclusive -> "CGB"
  putStrLn $ case sgbSupport of
    ROM.GBOnly -> ""
    ROM.UsesSGB -> "+SGB"

  let cartridge =
        "Cartridge: " ++ case ROM.mbcType cartridgeType of
          Nothing -> "No MBC"
          Just ROM.MBC1 -> "MBC1"
          Just ROM.MBC2 -> "MBC2"
          Just ROM.MBC3 -> "MBC3"
          Just ROM.MBC3RTC -> "MBC3+RTC"
          Just ROM.MBC5 -> "MBC5"
  putStrLn
    ( cartridge
        ++ (if ROM.hasSRAM cartridgeType then "+SRAM" else "")
        ++ (if ROM.hasBackupBattery cartridgeType then "+Battery" else "")
        ++ " ("
        ++ formatByteCount romSize
        ++ " ROM"
        ++ (if externalRAM > 0 then " + " ++ formatByteCount externalRAM ++ " RAM)" else ")")
    )

  putStrLn ("Start address: " ++ formatHex startAddress)
  putStrLn ""

formatByteCount :: Int -> String
formatByteCount b
  | b < 1024 = show b
  | b < 1024 * 1024 = show (b `div` 1024) ++ "KiB"
  | otherwise = show (b `div` (1024 * 1024)) ++ "MiB"
