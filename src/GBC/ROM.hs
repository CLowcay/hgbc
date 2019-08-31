{-# LANGUAGE RecordWildCards #-}

module GBC.ROM where

import           Data.Binary.Get
import           Data.Word
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Control.Monad

newtype ROM = ROM B.ByteString deriving (Eq, Ord, Show)

data CGBSupport = CGBIncompatible | CGBCompatible | CGBExclusive deriving (Eq, Ord, Show, Bounded, Enum)
data SGBSupport = GBOnly | UsesSGB deriving (Eq, Ord, Show, Bounded, Enum)
data MBCType = MBC1 | MBC2 | MBC3 | MBC3RTC | MBC5 deriving (Eq, Ord, Show, Bounded, Enum)
data Destination = Japan | Overseas deriving (Eq, Ord, Show, Bounded, Enum)

data CartridgeType = CartridgeType {
    mbcType :: Maybe MBCType
  , hasSRAM :: Bool
  , hasBackupBattery :: Bool
} deriving (Eq, Ord, Show)

data Header = Header {
    startAddress :: Word16
  , nintendoCharacterData :: B.ByteString
  , gameTitle :: B.ByteString
  , gameCode :: B.ByteString
  , cgbSupport :: CGBSupport
  , makerCode :: B.ByteString
  , sgbSupport :: SGBSupport
  , cartridgeType :: CartridgeType
  , romSize :: Int
  , externalRAM :: Int
  , destination :: Destination
  , oldLicenseCode :: Word8
  , maskROMVersion :: Int
} deriving (Eq, Ord, Show)

validateROM :: B.ByteString -> Either String ROM
validateROM rom = do
  when (rom `B.index` 0x100 /= 0x00) $ Left "Header check 0x100 failed"
  when (rom `B.index` 0x101 /= 0xC3) $ Left "Header check 0x101 failed"
  when (complementCheck rom /= 0) $ Left "Complement check failed"
  pure $ ROM rom
  where complementCheck = B.foldl' (+) 0x19 . B.drop 0x134 . B.take 0x14E

extractHeader :: ROM -> Header
extractHeader (ROM rom) =
  runGet getHeader . LB.fromStrict . B.take 0x50 . B.drop 0x100 $ rom
 where
  getHeader :: Get Header
  getHeader = do
    skip 2
    startAddress          <- getWord16le
    nintendoCharacterData <- getByteString 48
    gameTitle             <- fst . B.spanEnd (== 0) <$> getByteString 11
    gameCode              <- getByteString 4
    cgbSupport            <- decodeCGBSupport =<< getWord8
    makerCode             <- getByteString 2
    sgbSupport            <- decodeSGBSupport =<< getWord8
    cartridgeType         <- decodeCartridgeType =<< getWord8
    romSize               <- decodeROMSize =<< getWord8
    externalRAM           <- decodeExternalRAM =<< getWord8
    destination           <- decodeDestination =<< getWord8
    oldLicenseCode        <- getWord8
    maskROMVersion        <- fromIntegral <$> getWord8
    pure Header { .. }
  decodeCGBSupport b = case b of
    0x00 -> pure CGBIncompatible
    0x80 -> pure CGBCompatible
    0xC0 -> pure CGBExclusive
    x    -> fail $ "unknown cgb support code " ++ show x
  decodeSGBSupport b = case b of
    0x00 -> pure GBOnly
    0x03 -> pure UsesSGB
    x    -> fail $ "unknown sgb support code " ++ show x
  decodeDestination b = case b of
    0x00 -> pure Japan
    0x01 -> pure Overseas
    x    -> fail $ "unknown destination code " ++ show x
  decodeExternalRAM b = case b of
    0x00 -> pure 0
    0x02 -> pure $ 64 * 128
    0x03 -> pure $ 256 * 128
    0x04 -> pure $ 1024 * 128
    x    -> fail $ "unknown external RAM code " ++ show x
  decodeROMSize b = case b of
    0x00 -> pure $ 256 * 128
    0x01 -> pure $ 512 * 128
    0x02 -> pure $ 1024 * 128
    0x03 -> pure $ 2048 * 128
    0x04 -> pure $ 4096 * 128
    0x05 -> pure $ 8192 * 128
    0x06 -> pure $ 16 * 1024 * 128
    0x07 -> pure $ 32 * 1024 * 128
    0x08 -> pure $ 64 * 1024 * 128
    x    -> fail $ "unknown ROM size code " ++ show x
  decodeCartridgeType b = case b of
    0x00 -> pure $ CartridgeType Nothing False False
    0x01 -> pure $ CartridgeType (Just MBC1) False False
    0x02 -> pure $ CartridgeType (Just MBC1) True False
    0x03 -> pure $ CartridgeType (Just MBC1) True True
    0x04 -> fail "invalid cartridgeType 0x04"
    0x05 -> pure $ CartridgeType (Just MBC2) False False
    0x06 -> pure $ CartridgeType (Just MBC2) False True
    0x07 -> fail "invalid cartridgeType 0x07"
    0x08 -> pure $ CartridgeType Nothing True False
    0x09 -> pure $ CartridgeType Nothing True True
    0x0F -> pure $ CartridgeType (Just MBC3) False True
    0x10 -> pure $ CartridgeType (Just MBC3) True True
    0x11 -> pure $ CartridgeType (Just MBC3RTC) False False
    0x12 -> pure $ CartridgeType (Just MBC3RTC) True False
    0x13 -> pure $ CartridgeType (Just MBC3RTC) True True
    0x19 -> pure $ CartridgeType (Just MBC5) False False
    0x1A -> pure $ CartridgeType (Just MBC5) True False
    0x1B -> pure $ CartridgeType (Just MBC5) True True
    x    -> fail $ "unknown cartridge type code " ++ show x
