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
import qualified Data.ByteString.Unsafe        as B

data VideoBuffers = VideoBuffers {
    vram :: !(Ptr Word8)
  , oam :: !(Ptr Int32)
  , registers :: !(Ptr Int32)
}

data Memory = Memory {
    memRom :: !B.ByteString
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
initMemory (ROM memRom) videoBuffer = do
  memRam <- mallocForeignPtrArray 0x7FFF
  pure Memory { .. }

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = extractHeader (ROM memRom)

-- | Copy data to OAM memory via DMA.
-- TODO: Cannot use moveArray, have to expand the bytes to ints.
{-# INLINE dmaToOAM #-}
dmaToOAM :: HasMemory env => Word16 -> ReaderT env IO ()
dmaToOAM source = do
  Memory {..} <- asks forMemory
  liftIO $ case source `unsafeShiftR` 13 of
    0 -> B.unsafeUseAsCString memRom $ \ptr -> copyOAM (oam videoBuffer) (castPtr ptr)
    1 -> B.unsafeUseAsCString memRom $ \ptr -> copyOAM (oam videoBuffer) (castPtr ptr)
    2 -> B.unsafeUseAsCString memRom $ \ptr -> copyOAM (oam videoBuffer) (castPtr ptr)
    3 -> B.unsafeUseAsCString memRom $ \ptr -> copyOAM (oam videoBuffer) (castPtr ptr)
    4 -> copyOAM (oam videoBuffer) (vram videoBuffer `plusPtr` (fromIntegral source - 0x8000))
    5 -> withForeignPtr memRam
      $ \ram -> copyOAM (oam videoBuffer) (ram `plusPtr` (fromIntegral source - 0x8000))
    6 -> withForeignPtr memRam
      $ \ram -> copyOAM (oam videoBuffer) (ram `plusPtr` (fromIntegral source - 0x8000))
    _ -> error ("Invalid source for DMA " ++ show source)
 where
  copyOAM :: Ptr Int32 -> Ptr Word8 -> IO ()
  copyOAM to from =
    for_ [0 .. 159] $ \off -> pokeElemOff to off . fromIntegral =<< peekElemOff from off

-- | Read a byte from memory.
{-# INLINABLE readByte #-}
readByte :: HasMemory env => Word16 -> ReaderT env IO Word8
readByte addr = do
  Memory {..} <- asks forMemory
  case addr `unsafeShiftR` 13 of
    0 -> pure (memRom `B.unsafeIndex` romOffset)
    1 -> pure (memRom `B.unsafeIndex` romOffset)
    2 -> pure (memRom `B.unsafeIndex` romOffset)
    3 -> pure (memRom `B.unsafeIndex` romOffset)
    4 -> liftIO (peekElemOff (vram videoBuffer) ramOffset)
    5 -> liftIO (withForeignPtr memRam (`peekElemOff` ramOffset))
    6 -> liftIO (withForeignPtr memRam (`peekElemOff` ramOffset))
    7 -> liftIO $ if addr >= 0xFE00 && addr < 0xFEA0
      then do
        value <- peekElemOff (oam videoBuffer) oamOffset
        pure (fromIntegral value)
      else if addr >= 0xFF40 && addr <= 0xFF4B
        then do
          value <- peekElemOff (registers videoBuffer) registerOffset
          pure (fromIntegral value)
        else withForeignPtr memRam (`peekElemOff` ramOffset)
    x -> error ("Impossible coarse read address" ++ show x)
 where
  romOffset      = fromIntegral addr
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
  case addr `unsafeShiftR` 13 of
    0 -> pure ()
    1 -> pure ()
    2 -> pure ()
    3 -> pure ()
    4 -> liftIO (pokeElemOff (vram videoBuffer) ramOffset value)
    5 -> liftIO (withForeignPtr memRam $ \ptr -> pokeElemOff ptr ramOffset value)
    6 -> liftIO (withForeignPtr memRam $ \ptr -> pokeElemOff ptr ramOffset value)
    7 -> liftIO $ if addr >= 0xFE00 && addr < 0xFEA0
      then pokeElemOff (oam videoBuffer) oamOffset (fromIntegral value)
      else if addr >= 0xFF40 && addr <= 0xFF4B
        then pokeElemOff (registers videoBuffer) registerOffset (fromIntegral value)
        else withForeignPtr memRam $ \ptr -> pokeElemOff ptr ramOffset value
    x -> error ("Impossible coarse read address" ++ show x)
 where
  ramOffset      = fromIntegral addr - 0x8000
  oamOffset      = fromIntegral addr - 0xFE00
  registerOffset = fromIntegral addr - 0xFF40

-- | Read a chunk of memory.
{-# INLINABLE readChunk #-}
readChunk :: HasMemory env => Word16 -> Int -> ReaderT env IO B.ByteString
readChunk base len = B.pack <$> traverse readByte [base .. base + fromIntegral len - 1]
