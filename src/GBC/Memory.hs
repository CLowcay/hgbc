{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Memory
  ( Memory
  , HasMemory(..)
  , initMemory
  , getROMHeader
  , shouldCheckRAMAccess
  , getMbcRegisters
  , dmaToOAM
  , readByte
  , writeByte
  , writeWord
  , copy16
  , readChunk
  )
where

import           Common
import           Control.Exception              ( throwIO )
import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Errors
import           GBC.Graphics.VRAM
import           GBC.Mode
import           GBC.Primitive
import           GBC.ROM
import           GBC.Registers
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B
import qualified Data.Vector                   as V
import           Data.Bifunctor

data Memory = Memory {
    mbc            :: !MBC
  , romHeader      :: !Header
  , rom            :: !B.ByteString
  , memRam         :: !(ForeignPtr Word8)
  , ramBankOffset  :: !(IORef Int)
  , ports          :: !(V.Vector (Port Word8))
  , portIE         :: !(Port Word8)
  , memHigh        :: !(ForeignPtr Word8)
  , mode           :: !EmulatorMode
  , vram           :: !VRAM
  , checkRAMAccess :: !(IORef Bool)
}

class HasMemory env where
  forMemory :: env -> Memory

instance HasMemory Memory where
  {-# INLINE forMemory #-}
  forMemory = id

portOffset :: Word16 -> Int
portOffset = subtract 0xFF00 . fromIntegral

-- | The initial memory state.
initMemory :: ROM -> VRAM -> [(Word16, Port Word8)] -> Port Word8 -> EmulatorMode -> IO Memory
initMemory romInfo@(ROM _ romHeader rom) vram rawPorts portIE mode = do
  memRam        <- mallocForeignPtrArray 0x10000
  memHigh       <- mallocForeignPtrArray 0x80
  emptyPort     <- newPort 0xFF 0x00 neverUpdate

  ramBankOffset <- newIORef 0
  svbk          <- newPort 0xF8 0x07 $ \_ newValue -> do
    let bank = fromIntegral (newValue .&. 7)
    writeIORef ramBankOffset $! 0 `max` ((bank - 1) * 0x1000)
    pure newValue

  let ports = V.accum (flip const)
                      (V.replicate 128 emptyPort)
                      (first portOffset <$> ((SVBK, svbk) : rawPorts))

  mbc            <- getMBC romInfo
  checkRAMAccess <- newIORef False
  pure Memory { .. }

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = romHeader

-- | Check that RAM access operations are valid.
shouldCheckRAMAccess :: HasMemory env => Bool -> ReaderT env IO ()
shouldCheckRAMAccess check = do
  Memory {..} <- asks forMemory
  liftIO (writeIORef checkRAMAccess check)

getMbcRegisters :: HasMemory env => ReaderT env IO [RegisterInfo]
getMbcRegisters = do
  Memory {..} <- asks forMemory
  liftIO (mbcRegisters mbc)

-- | Copy data to OAM memory via DMA.
-- TODO: Cannot use moveArray, have to expand the bytes to ints.
{-# INLINE dmaToOAM #-}
dmaToOAM :: HasMemory env => Word16 -> ReaderT env IO ()
dmaToOAM source = do
  Memory {..} <- asks forMemory
  liftIO $ case source `unsafeShiftR` 13 of
    0 -> B.unsafeUseAsCString rom (copyToOAM vram . (`plusPtr` fromIntegral source))
    1 -> B.unsafeUseAsCString rom (copyToOAM vram . (`plusPtr` fromIntegral source))
    2 -> do
      bank <- bankOffset mbc
      B.unsafeUseAsCString rom (copyToOAM vram . (`plusPtr` (bank + offset 0x4000)))
    3 -> do
      bank <- bankOffset mbc
      B.unsafeUseAsCString rom (copyToOAM vram . (`plusPtr` (bank + offset 0x4000)))
    4 -> copyVRAMToOAM vram source
    5 -> do
      check <- liftIO (readIORef checkRAMAccess)
      withRAMPointer mbc check (source - 0xA000) (copyToOAM vram)
    6
      | source < 0xD000 || mode == DMG -> withForeignPtr memRam
      $  \ram -> copyToOAM vram (ram `plusPtr` offset 0xC000)
      | otherwise -> do
        bankOffset <- readIORef ramBankOffset
        withForeignPtr memRam $ \ram -> copyToOAM vram (ram `plusPtr` (bankOffset + offset 0xC000))
    _ -> liftIO (throwIO (InvalidSourceForDMA source))
  where offset base = fromIntegral source - base

-- | Read a byte from memory.
{-# INLINABLE readByte #-}
readByte :: HasMemory env => Word16 -> ReaderT env IO Word8
readByte addr = do
  Memory {..} <- asks forMemory
  liftIO $ case addr `unsafeShiftR` 13 of
    0 -> pure $ rom `B.unsafeIndex` fromIntegral addr
    1 -> pure $ rom `B.unsafeIndex` fromIntegral addr
    2 -> do
      bank <- bankOffset mbc
      pure $ rom `B.unsafeIndex` (bank + offset 0x4000)
    3 -> do
      bank <- bankOffset mbc
      pure $ rom `B.unsafeIndex` (bank + offset 0x4000)
    4 -> readVRAM vram addr
    5 -> do
      check <- liftIO (readIORef checkRAMAccess)
      readRAM mbc check (addr - 0xA000)
    6
      | addr < 0xD000 || mode == DMG -> withForeignPtr memRam (`peekElemOff` offset 0xC000)
      | otherwise -> do
        bankOffset <- readIORef ramBankOffset
        withForeignPtr memRam (`peekElemOff` (bankOffset + offset 0xC000))
    7
      | addr < 0xFE00 -> withForeignPtr memRam (`peekElemOff` offset 0xE000)
      | addr < 0xFEA0 -> do
        value <- readOAM vram addr
        pure (fromIntegral value)
      | addr < 0xFF00 -> liftIO $ do
        check <- readIORef checkRAMAccess
        if check then throwIO (InvalidRead (addr + 0xE000)) else pure 0xFF
      | addr < 0xFF80 -> liftIO $ readPort (ports V.! offset 0xFF00)
      | addr == IE -> liftIO $ readPort portIE
      | otherwise -> withForeignPtr memHigh (`peekElemOff` offset 0xFF80)
    x -> error ("Impossible coarse read address" ++ show x)
  where offset base = fromIntegral addr - base

{-# INLINABLE writeWord #-}
writeWord :: HasMemory env => Word16 -> Word16 -> ReaderT env IO ()
writeWord addr value = do
  writeByte addr       (fromIntegral (value .&. 0xFF))
  writeByte (addr + 1) (fromIntegral (value `unsafeShiftR` 8))

-- | Write to memory.
{-# INLINABLE writeByte #-}
writeByte :: HasMemory env => Word16 -> Word8 -> ReaderT env IO ()
writeByte addr value = do
  Memory {..} <- asks forMemory
  liftIO $ case addr `unsafeShiftR` 13 of
    0 -> writeROM mbc addr value
    1 -> writeROM mbc addr value
    2 -> writeROM mbc addr value
    3 -> writeROM mbc addr value
    4 -> writeVRAM vram addr value
    5 -> do
      check <- liftIO (readIORef checkRAMAccess)
      writeRAM mbc check (addr - 0xA000) value
    6
      | addr < 0xD000 || mode == DMG -> withForeignPtr memRam
      $  \ptr -> pokeElemOff ptr (offset 0xC000) value
      | otherwise -> do
        bankOffset <- readIORef ramBankOffset
        withForeignPtr memRam $ \ptr -> pokeElemOff ptr (bankOffset + offset 0xC000) value
    7
      | addr < 0xFE00 -> withForeignPtr memRam $ \ptr -> pokeElemOff ptr (offset 0xE000) value
      | addr < 0xFEA0 -> writeOAM vram addr value
      | addr < 0xFF00 -> do
        check <- readIORef checkRAMAccess
        if check then throwIO (InvalidWrite (addr + 0xE000)) else pure ()
      | addr < 0xFF80 -> writePort (ports V.! offset 0xFF00) value
      | addr == IE -> liftIO $ writePort portIE value
      | otherwise -> withForeignPtr memHigh $ \ptr -> pokeElemOff ptr (offset 0xFF80) value
    x -> error ("Impossible coarse read address" ++ show x)
  where offset base = fromIntegral addr - base

{-# INLINABLE copy16 #-}
copy16 :: HasMemory env => Word16 -> Word16 -> ReaderT env IO ()
copy16 source destination = do
  writeByte destination =<< readByte source
  writeByte (destination + 1) =<< readByte (source + 1)
  writeByte (destination + 2) =<< readByte (source + 2)
  writeByte (destination + 3) =<< readByte (source + 3)
  writeByte (destination + 4) =<< readByte (source + 4)
  writeByte (destination + 5) =<< readByte (source + 5)
  writeByte (destination + 6) =<< readByte (source + 6)
  writeByte (destination + 7) =<< readByte (source + 7)
  writeByte (destination + 8) =<< readByte (source + 8)
  writeByte (destination + 9) =<< readByte (source + 9)
  writeByte (destination + 10) =<< readByte (source + 10)
  writeByte (destination + 11) =<< readByte (source + 11)
  writeByte (destination + 12) =<< readByte (source + 12)
  writeByte (destination + 13) =<< readByte (source + 13)
  writeByte (destination + 14) =<< readByte (source + 14)
  writeByte (destination + 15) =<< readByte (source + 15)

-- | Read a chunk of memory.
{-# INLINABLE readChunk #-}
readChunk :: HasMemory env => Word16 -> Int -> ReaderT env IO B.ByteString
readChunk base len = B.pack <$> traverse readByte ((base +) <$> [0 .. fromIntegral len - 1])
