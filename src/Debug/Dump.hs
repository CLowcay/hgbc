{-# LANGUAGE RecordWildCards #-}
module Debug.Dump
  ( dumpRegisters
  , dumpHeader
  , dumpDisassembly
  , dumpMem
  , dumpGraphics
  , dumpTimer
  , dumpInternal
  , dumpMBC
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.Word
import           GBC.CPU
import           GBC.Decode
import           GBC.Graphics
import           GBC.ISA
import           GBC.Memory
import           GBC.ROM
import           GBC.Timer
import           System.Console.Haskeline
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

dumpRegisters :: HasCPU env => InputT (ReaderT env IO) ()
dumpRegisters = do
  RegisterFile {..} <- lift getRegisterFile
  outputStrLn
    ("F=" ++ formatHex regF ++ " A=" ++ formatHex regA ++ " AF=" ++ formatHex (assemble regA regF))
  outputStrLn
    ("B=" ++ formatHex regB ++ " C=" ++ formatHex regC ++ " BC=" ++ formatHex (assemble regB regC))
  outputStrLn
    ("D=" ++ formatHex regD ++ " E=" ++ formatHex regE ++ " DE=" ++ formatHex (assemble regD regE))
  outputStrLn
    ("H=" ++ formatHex regH ++ " L=" ++ formatHex regL ++ " HL=" ++ formatHex (assemble regH regL))
  outputStrLn ("PC=" ++ formatHex regPC ++ "   SP=" ++ formatHex regSP)
  outputStrLn
    (  "flags="
    ++ [ if regF .&. flagZ /= 0 then 'Z' else 'z'
       , if regF .&. flagN /= 0 then 'N' else 'n'
       , if regF .&. flagH /= 0 then 'H' else 'h'
       , if regF .&. flagCY /= 0 then 'C' else 'c'
       , if regHidden .&. flagIME /= 0 then 'I' else 'i'
       ]
    )
  where assemble h l = (fromIntegral h `unsafeShiftL` 8 .|. fromIntegral l) :: Word16

dumpHeader :: Header -> InputT (ReaderT env IO) ()
dumpHeader Header {..} = do
  outputStrLn
    $  take 11 (BC.unpack gameTitle ++ repeat ' ')
    ++ BC.unpack gameCode
    ++ " "
    ++ case destination of
         Japan    -> " (JAPAN)"
         Overseas -> " (INTERNATIONAL)"

  outputStrLn ("Version: " ++ show maskROMVersion)
  outputStrLn ("Maker: " ++ formatHex oldLicenseCode ++ " " ++ BC.unpack makerCode)

  outputStr $ "Console support: " ++ case cgbSupport of
    CGBIncompatible -> "GB"
    CGBCompatible   -> "GB+CGB"
    CGBExclusive    -> "CGB"
  outputStrLn $ case sgbSupport of
    GBOnly  -> ""
    UsesSGB -> "+SGB"

  let cartridge = "Cartridge: " ++ case mbcType cartridgeType of
        Nothing      -> "No MBC"
        Just MBC1    -> "MBC1"
        Just MBC2    -> "MBC2"
        Just MBC3    -> "MBC3"
        Just MBC3RTC -> "MBC3+RTC"
        Just MBC5    -> "MBC5"
  outputStrLn
    (  cartridge
    ++ (if hasSRAM cartridgeType then "+SRAM" else "")
    ++ (if hasBackupBattery cartridgeType then "+Battery" else "")
    ++ " ("
    ++ formatByteCount romSize
    ++ " ROM"
    ++ (if externalRAM > 0 then " + " ++ formatByteCount externalRAM ++ " RAM)" else ")")
    )

  outputStrLn ("Start address: " ++ formatHex startAddress)

formatByteCount :: Int -> String
formatByteCount b | b < 1024        = show b
                  | b < 1024 * 1024 = show (b `div` 1024) ++ "KiB"
                  | otherwise       = show (b `div` (1024 * 1024)) ++ "MiB"

dumpDisassembly
  :: HasMemory env
  => (Word16 -> IO String)
  -> SymbolTable
  -> Word16
  -> Int
  -> InputT (ReaderT env IO) ()
dumpDisassembly decorator symbolTable base n = do
  instructions <- lift (decodeN base n)
  for_ instructions $ \(addr, instruction) -> do
    decoration <- liftIO (decorator addr)
    case lookupByAddress symbolTable addr of
      Nothing    -> pure ()
      Just label -> outputStrLn ((' ' <$ decoration) ++ label ++ ":")
    outputStrLn
      (  decoration
      ++ formatHex addr
      ++ ": "
      ++ formatWithSymbolTable symbolTable instruction
      ++ extraInfo addr instruction
      )
 where
  extraInfo addr (JR e) = " [" ++ formatOrLookup16 symbolTable (addr + 2 + fromIntegral e) ++ "]"
  extraInfo addr (JRCC _ e) =
    " [" ++ formatOrLookup16 symbolTable (addr + 2 + fromIntegral e) ++ "]"
  extraInfo _ _ = ""

dumpMem :: HasMemory env => Word16 -> InputT (ReaderT env IO) ()
dumpMem base = for_ [0 .. 15] $ \line ->
  let offset = base + line * 16 in outputStrLn . hexDump offset =<< lift (readChunk offset 16)

hexDump :: Word16 -> B.ByteString -> String
hexDump base lineData =
  formatHex base
    ++ ": "
    ++ unwords (formatHex <$> B.unpack lineData)
    ++ " "
    ++ (toPrintable <$> BC.unpack lineData)

toPrintable :: Char -> Char
toPrintable c = if c <= ' ' || c >= '\DEL' then '.' else c

dumpGraphics :: HasMemory env => Maybe String -> InputT (ReaderT env IO) ()
dumpGraphics register = dumpRegisterInfo . filterRegister register =<< lift graphicsRegisters

dumpTimer :: HasTimer env => Maybe String -> InputT (ReaderT env IO) ()
dumpTimer register = dumpRegisterInfo . filterRegister register =<< lift timerRegisters

dumpInternal :: HasMemory env => Maybe String -> InputT (ReaderT env IO) ()
dumpInternal register = dumpRegisterInfo . filterRegister register =<< lift internalRegisters

dumpMBC :: HasMemory env => Maybe String -> InputT (ReaderT env IO) ()
dumpMBC register = dumpRegisterInfo . filterRegister register =<< lift getMbcRegisters

filterRegister :: Maybe String -> [RegisterInfo] -> [RegisterInfo]
filterRegister Nothing  = id
filterRegister (Just r) = filter isRegister where isRegister (RegisterInfo _ i _ _) = r == i

dumpRegisterInfo :: MonadIO m => [RegisterInfo] -> InputT m ()
dumpRegisterInfo = traverse_ dumpRegister
 where
  dumpRegister (RegisterInfo address name value flags) = do
    outputStrLn (formatHex address ++ " " ++ padLeft 4 ' ' name ++ " = " ++ formatHex value)
    for_ flags $ \(flag, status) -> outputStrLn ("       " ++ flag ++ ": " ++ status)
