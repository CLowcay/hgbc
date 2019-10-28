{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Memory
  ( Memory
  , VideoBuffers(..)
  , HasMemory(..)
  , initMemory
  , getROMHeader
  , dmaToOAM
  , readByte
  , writeByte
  , writeWord
  , readChunk
  )
where

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
  memRam <- mallocForeignPtrArray 0x7FFF
  mbc    <- getMBC rom
  pure Memory { .. }

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = romHeader

-- | Copy data to OAM memory via DMA.
-- TODO: Cannot use moveArray, have to expand the bytes to ints.
{-# INLINE dmaToOAM #-}
dmaToOAM :: HasMemory env => Word16 -> ReaderT env IO ()
dmaToOAM source = do
  Memory {..} <- asks forMemory
  let copyOAM from = for_ [0 .. 159] $ \off ->
        pokeElemOff (oam videoBuffer) off . fromIntegral =<< peekElemOff (from :: Ptr Word8) off

  liftIO $ case source `unsafeShiftR` 13 of
    0 -> withROMPointer mbc source copyOAM
    1 -> withROMPointer mbc source copyOAM
    2 -> withROMPointer mbc source copyOAM
    3 -> withROMPointer mbc source copyOAM
    4 -> copyOAM (vram videoBuffer `plusPtr` (fromIntegral source - 0x8000))
    5 -> withRAMPointer mbc (source - 0x8000) copyOAM
    6 -> withForeignPtr memRam $ \ram -> copyOAM (ram `plusPtr` (fromIntegral source - 0x8000))
    _ -> error ("Invalid source for DMA " ++ show source)

-- | Read a byte from memory.
{-# INLINABLE readByte #-}
readByte :: HasMemory env => Word16 -> ReaderT env IO Word8
readByte addr = do
  Memory {..} <- asks forMemory
  liftIO $ case addr `unsafeShiftR` 13 of
    0 -> readROM mbc addr
    1 -> readROM mbc addr
    2 -> readROM mbc addr
    3 -> readROM mbc addr
    4 -> peekElemOff (vram videoBuffer) ramOffset
    5 -> readRAM mbc (addr - 0x8000)
    6 -> withForeignPtr memRam (`peekElemOff` ramOffset)
    7
      | addr >= 0xFE00 && addr <= 0xFE9F -> do
        value <- peekElemOff (oam videoBuffer) oamOffset
        pure (fromIntegral value)
      | addr >= 0xFF40 && addr <= 0xFF4B -> do
        value <- peekElemOff (registers videoBuffer) registerOffset
        pure (fromIntegral value)
      | otherwise -> withForeignPtr memRam (`peekElemOff` ramOffset)
    x -> error ("Impossible coarse read address" ++ show x)
 where
  ramOffset      = fromIntegral addr - 0x8000
  oamOffset      = fromIntegral addr - 0xFE00
  registerOffset = fromIntegral addr - 0xFF40

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
    4 -> pokeElemOff (vram videoBuffer) ramOffset value
    5 -> writeRAM mbc addr value
    6 -> withForeignPtr memRam $ \ptr -> pokeElemOff ptr ramOffset value
    7
      | addr >= 0xFE00 && addr <= 0xFE9F -> pokeElemOff (oam videoBuffer)
                                                        oamOffset
                                                        (fromIntegral value)
      | addr >= 0xFF40 && addr <= 0xFF4B -> pokeElemOff (registers videoBuffer)
                                                        registerOffset
                                                        (fromIntegral value)
      | otherwise -> withForeignPtr memRam $ \ptr -> pokeElemOff ptr ramOffset value
    x -> error ("Impossible coarse read address" ++ show x)
 where
  ramOffset      = fromIntegral addr - 0x8000
  oamOffset      = fromIntegral addr - 0xFE00
  registerOffset = fromIntegral addr - 0xFF40

-- | Read a chunk of memory.
{-# INLINABLE readChunk #-}
readChunk :: HasMemory env => Word16 -> Int -> ReaderT env IO B.ByteString
readChunk base len = B.pack <$> traverse readByte [base .. base + fromIntegral len - 1]
