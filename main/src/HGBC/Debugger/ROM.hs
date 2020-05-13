{-# LANGUAGE RecordWildCards #-}
module HGBC.Debugger.ROM
  ( dumpHeader
  )
where

import           Machine.GBC.ROM
import           Machine.GBC.Util               ( formatHex )
import qualified Data.ByteString.Char8         as BC

dumpHeader :: Header -> IO ()
dumpHeader Header {..} = do
  putStrLn
    $  take 16 (BC.unpack gameTitle ++ repeat ' ')
    ++ " "
    ++ BC.unpack gameCode
    ++ " "
    ++ case destination of
         Japan    -> " (JAPAN)"
         Overseas -> " (INTERNATIONAL)"

  putStrLn ("Version: " ++ show maskROMVersion)
  putStrLn
    (  "Licensee code: "
    ++ if oldLicenseCode == 0x33 then BC.unpack makerCode else formatHex oldLicenseCode <> "h"
    )

  putStr $ "Console support: " ++ case cgbSupport of
    CGBIncompatible -> "GB"
    CGBCompatible   -> "GB+CGB"
    CGBExclusive    -> "CGB"
  putStrLn $ case sgbSupport of
    GBOnly  -> ""
    UsesSGB -> "+SGB"

  let cartridge = "Cartridge: " ++ case mbcType cartridgeType of
        Nothing      -> "No MBC"
        Just MBC1    -> "MBC1"
        Just MBC2    -> "MBC2"
        Just MBC3    -> "MBC3"
        Just MBC3RTC -> "MBC3+RTC"
        Just MBC5    -> "MBC5"
  putStrLn
    (  cartridge
    ++ (if hasSRAM cartridgeType then "+SRAM" else "")
    ++ (if hasBackupBattery cartridgeType then "+Battery" else "")
    ++ " ("
    ++ formatByteCount romSize
    ++ " ROM"
    ++ (if externalRAM > 0 then " + " ++ formatByteCount externalRAM ++ " RAM)" else ")")
    )

  putStrLn ("Start address: " ++ formatHex startAddress)
  putStrLn ""

formatByteCount :: Int -> String
formatByteCount b | b < 1024        = show b
                  | b < 1024 * 1024 = show (b `div` 1024) ++ "KiB"
                  | otherwise       = show (b `div` (1024 * 1024)) ++ "MiB"
