{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module GBC.ROM
  ( ROM(..)
  , CGBSupport(..)
  , SGBSupport(..)
  , MBCType(..)
  , Destination(..)
  , CartridgeType(..)
  , Header(..)
  , validateROM
  , extractHeader
  , MBC(..)
  , getMBC
  )
where

import           Control.Monad
import           Data.Binary.Get
import           Data.IORef
import           Common
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Unsafe        as B
import           Data.Bits

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
extractHeader (ROM rom) = runGet getHeader . LB.fromStrict . B.take 0x50 . B.drop 0x100 $ rom
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

-- | Get the Memory Bank Controller for this cartridge.
getMBC :: ROM -> IO MBC
getMBC rom =
  let cType = cartridgeType (extractHeader rom)
  in  case mbcType cType of
        Nothing   -> nullMBC rom
        Just MBC1 -> mbc1 rom
        Just x    -> error ("Unsupported " ++ show x)

-- | A memory bank controller.
data MBC = MBC {
    readROMLow :: Word16 -> IO Word8
  , readROMHigh :: Word16 -> IO Word8
  , writeROM :: Word16 -> Word8 -> IO ()
  , withROMLowPointer :: forall a. Word16 -> (Ptr Word8 -> IO a) -> IO a
  , withROMHighPointer :: forall a. Word16 -> (Ptr Word8 -> IO a) -> IO a
  , readRAM :: Word16 -> IO Word8
  , writeRAM :: Word16 -> Word8 -> IO ()
  , withRAMPointer :: forall a. Word16 -> (Ptr Word8 -> IO a) -> IO a
  , mbcRegisters :: IO [RegisterInfo]
}

-- | Simulate a cartridge with no memory bank controller.
nullMBC :: ROM -> IO MBC
nullMBC (ROM romData) = do
  ram <- mallocForeignPtrBytes 0x2000
  pure MBC
    { readROMLow         = \address -> pure (romData `B.unsafeIndex` fromIntegral address)
    , withROMLowPointer  = \address action -> B.unsafeUseAsCString
                             romData
                             (action . (`plusPtr` fromIntegral address))
    , readROMHigh = \address -> pure (romData `B.unsafeIndex` (fromIntegral address + 0x4000))
    , withROMHighPointer = \address action -> B.unsafeUseAsCString
                             romData
                             (action . (`plusPtr` (fromIntegral address + 0x4000)))
    , writeROM           = \_ _ -> pure ()
    , readRAM = \address -> withForeignPtr ram $ \ptr -> peekElemOff ptr (fromIntegral address)
    , writeRAM           = \address value ->
                             withForeignPtr ram $ \ptr -> pokeElemOff ptr (fromIntegral address) value
    , withRAMPointer     = \address action ->
                             withForeignPtr ram (action . (`plusPtr` fromIntegral address))
    , mbcRegisters       = pure []
    }

mbc1 :: ROM -> IO MBC
mbc1 (ROM romData) = do
  romOffset <- newIORef 1
  ramOffset <- newIORef 0
  enableRAM <- newIORef False
  ramSelect <- newIORef False
  ram       <- mallocForeignPtrBytes 0x8000 :: IO (ForeignPtr Word8)

  let getROMOffset = do
        noHighROM <- readIORef ramSelect
        low       <- readIORef romOffset
        if noHighROM
          then pure (low `unsafeShiftL` 14)
          else do
            high <- readIORef ramOffset
            pure (((high `unsafeShiftL` 5) .|. low) `unsafeShiftL` 14)
  let getRAMOffset = do
        ramBanking <- readIORef ramSelect
        if ramBanking
          then do
            offset <- readIORef ramOffset
            pure (offset `unsafeShiftL` 13)
          else pure 0

  pure MBC
    { readROMLow         = \address -> pure (romData `B.unsafeIndex` fromIntegral address)
    , readROMHigh        = \address -> do
                             offset <- getROMOffset
                             pure (romData `B.unsafeIndex` (offset + fromIntegral address))
    , writeROM           = \address value -> if address < 0x2000
                             then writeIORef enableRAM (value .&. 0x0F == 0x0A)
                             else if address < 0x4000
                               then
                                 let low = (fromIntegral value .&. 0x1F)
                                 in  writeIORef romOffset (if low == 0 then 1 else low)
                               else if address < 0x6000
                                 then writeIORef ramOffset (fromIntegral value .&. 0x3)
                                 else writeIORef ramSelect (value /= 0)
    , withROMLowPointer  = \address action -> B.unsafeUseAsCString
                             romData
                             (action . (`plusPtr` fromIntegral address))
    , withROMHighPointer = \address action -> do
                             offset <- getROMOffset
                             B.unsafeUseAsCString
                               romData
                               (action . (`plusPtr` (offset + fromIntegral address)))
    , readRAM            = \address -> do
                             offset <- getRAMOffset
                             withForeignPtr ram $ \ptr -> peekElemOff ptr (offset + fromIntegral address)
    , writeRAM           = \address value -> do
                             offset <- getRAMOffset
                             withForeignPtr ram
                               $ \ptr -> pokeElemOff ptr (offset + fromIntegral address) value
    , withRAMPointer     = \address action -> do
                             offset <- getRAMOffset
                             withForeignPtr ram (action . (`plusPtr` (offset + fromIntegral address)))
    , mbcRegisters       =
      do
        r1 <- readIORef romOffset
        r2 <- readIORef ramOffset
        r0 <- readIORef enableRAM
        r3 <- readIORef ramSelect
        pure
          [ RegisterInfo 0      "R0" (if r0 then 0x0A else 0) [("RAM enabled ", show r0)]
          , RegisterInfo 0x2000 "R1" (fromIntegral r1)        []
          , RegisterInfo 0x4000 "R2" (fromIntegral r2)        []
          , RegisterInfo 0x6000
                         "R3"
                         (if r3 then 1 else 0)
                         [("R2 is", if r3 then "RAM bank" else "ROM bank high bits")]
          ]
    }
