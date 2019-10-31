{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Memory
  ( Memory
  , VideoBuffers(..)
  , HasMemory(..)
  , initMemory
  , getROMHeader
  , getMbcRegisters
  , dmaToOAM
  , readByte
  , writeByte
  , writeWord
  , readChunk
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.Int
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.ROM
import qualified Data.ByteString               as B

data VideoBuffers = VideoBuffers {
    vram :: !(Ptr Word8)
  , oam :: !(Ptr Int32)
  , registers :: !(Ptr Int32)
}

data Memory = Memory {
    mbc :: !MBC
  , romHeader :: !Header
  , memRam :: !(ForeignPtr Word8)
  , memHigh :: !(ForeignPtr Word8)
  , videoBuffer :: !VideoBuffers
}

class HasMemory env where
  forMemory :: env -> Memory

instance HasMemory Memory where
  {-# INLINE forMemory #-}
  forMemory = id

-- | The initial memory state.
initMemory :: ROM -> VideoBuffers -> IO Memory
initMemory rom videoBuffer = do
  let romHeader = extractHeader rom
  memRam  <- mallocForeignPtrArray 0x2000
  memHigh <- mallocForeignPtrArray 0x100
  mbc     <- getMBC rom
  pure Memory { .. }

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = romHeader

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
  let copyOAM from = for_ [0 .. 159] $ \off ->
        pokeElemOff (oam videoBuffer) off . fromIntegral =<< peekElemOff (from :: Ptr Word8) off

  liftIO $ case source `unsafeShiftR` 13 of
    0 -> withROMLowPointer mbc source copyOAM
    1 -> withROMLowPointer mbc source copyOAM
    2 -> withROMHighPointer mbc (source - 0x4000) copyOAM
    3 -> withROMHighPointer mbc (source - 0x4000) copyOAM
    4 -> copyOAM (vram videoBuffer `plusPtr` offset 0x8000)
    5 -> withRAMPointer mbc (source - 0xA000) copyOAM
    6 -> withForeignPtr memRam $ \ram -> copyOAM (ram `plusPtr` offset 0xC000)
    _ -> error ("Invalid source for DMA " ++ show source)
  where offset base = fromIntegral source - base

-- | Read a byte from memory.
{-# INLINABLE readByte #-}
readByte :: HasMemory env => Word16 -> ReaderT env IO Word8
readByte addr = do
  Memory {..} <- asks forMemory
  liftIO $ case addr `unsafeShiftR` 13 of
    0 -> readROMLow mbc addr
    1 -> readROMLow mbc addr
    2 -> readROMHigh mbc (addr - 0x4000)
    3 -> readROMHigh mbc (addr - 0x4000)
    4 -> peekElemOff (vram videoBuffer) (offset 0x8000)
    5 -> readRAM mbc (addr - 0xA000)
    6 -> withForeignPtr memRam (`peekElemOff` offset 0xC000)
    7
      | addr < 0xFE00 -> withForeignPtr memRam (`peekElemOff` offset 0xE000)
      | addr < 0xFEA0 -> do
        value <- peekElemOff (oam videoBuffer) (offset 0xFE00)
        pure (fromIntegral value)
      | addr < 0xFF00 -> pure 0xFF
      | addr >= 0xFF40 && addr <= 0xFF4B -> do
        value <- peekElemOff (registers videoBuffer) (offset 0xFF40)
        pure (fromIntegral value)
      | otherwise -> withForeignPtr memHigh (`peekElemOff` offset 0xFF00)
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
    4 -> pokeElemOff (vram videoBuffer) (offset 0x8000) value
    5 -> writeRAM mbc (addr - 0xA000) value
    6 -> withForeignPtr memRam $ \ptr -> pokeElemOff ptr (offset 0xC000) value
    7
      | addr < 0xFE00 -> withForeignPtr memRam $ \ptr -> pokeElemOff ptr (offset 0xE000) value
      | addr < 0xFEA0 -> pokeElemOff (oam videoBuffer) (offset 0xFE00) (fromIntegral value)
      | addr < 0xFF00 -> pure ()
      | addr >= 0xFF40 && addr <= 0xFF4B -> pokeElemOff (registers videoBuffer)
                                                        (offset 0xFF40)
                                                        (fromIntegral value)
      | otherwise -> withForeignPtr memHigh $ \ptr -> pokeElemOff ptr (offset 0xFF00) value
    x -> error ("Impossible coarse read address" ++ show x)
  where offset base = fromIntegral addr - base

-- | Read a chunk of memory.
{-# INLINABLE readChunk #-}
readChunk :: HasMemory env => Word16 -> Int -> ReaderT env IO B.ByteString
readChunk base len = B.pack <$> traverse readByte [base .. base + fromIntegral len - 1]
