{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Memory
  ( Memory
  , VideoBuffers(..)
  , HasMemory(..)
  , UsesMemory
  , initMemory
  , getROMHeader
  , dmaToOAM
  , readByte
  , writeMem
  , readChunk
  )
where

import           Data.Bits
import           Control.Monad.Reader
import           Data.Word
import           Data.Int
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.ROM
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B

data VideoBuffers = VideoBuffers {
    vram :: !(Ptr Word8)
  , oam :: !(Ptr Word8)
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

-- | Constraints for monads that can access memory.
type UsesMemory env m = (MonadIO m, HasMemory env)

-- | The initial memory state.
initMemory :: ROM -> VideoBuffers -> IO Memory
initMemory (ROM memRom) videoBuffer = do
  memRam <- mallocForeignPtrArray 0x7FFF
  pure Memory { .. }

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = extractHeader $ ROM memRom

-- | Copy data to OAM memory via DMA.
dmaToOAM :: UsesMemory env m => Word16 -> ReaderT env m ()
dmaToOAM source = do
  Memory {..} <- asks forMemory
  liftIO $ case source .&. 0xE000 of
    0x0000 -> B.unsafeUseAsCString memRom $ \ptr -> moveArray (oam videoBuffer) (castPtr ptr) 160
    0x2000 -> B.unsafeUseAsCString memRom $ \ptr -> moveArray (oam videoBuffer) (castPtr ptr) 160
    0x4000 -> B.unsafeUseAsCString memRom $ \ptr -> moveArray (oam videoBuffer) (castPtr ptr) 160
    0x6000 -> B.unsafeUseAsCString memRom $ \ptr -> moveArray (oam videoBuffer) (castPtr ptr) 160
    0x8000 ->
      moveArray (oam videoBuffer) (vram videoBuffer `plusPtr` (fromIntegral source - 0x8000)) 160
    0xA000 -> withForeignPtr memRam
      $ \ram -> moveArray (oam videoBuffer) (ram `plusPtr` (fromIntegral source - 0x8000)) 160
    0xC000 -> withForeignPtr memRam
      $ \ram -> moveArray (oam videoBuffer) (ram `plusPtr` (fromIntegral source - 0x8000)) 160
    _ -> error ("Invalid source for DMA " ++ show source)

-- | Read a byte from memory.
{-# INLINE readByte #-}
readByte :: UsesMemory env m => Word16 -> ReaderT env m Word8
readByte addr = do
  Memory {..} <- asks forMemory
  case addr .&. 0xE000 of
    0x0000 -> pure (memRom `B.index` romOffset)
    0x2000 -> pure (memRom `B.index` romOffset)
    0x4000 -> pure (memRom `B.index` romOffset)
    0x6000 -> pure (memRom `B.index` romOffset)
    0x8000 -> liftIO (peekByteOff (vram videoBuffer) ramOffset)
    0xA000 -> liftIO (withForeignPtr memRam (`peekByteOff` ramOffset))
    0xC000 -> liftIO (withForeignPtr memRam (`peekByteOff` ramOffset))
    0xE000 -> liftIO (withForeignPtr memRam (`peekByteOff` ramOffset))
    x      -> error ("Impossible coarse read address" ++ show x)
 where
  romOffset = fromIntegral addr
  ramOffset = fromIntegral addr - 0x8000

-- | Write to memory.
{-# INLINE writeMem #-}
writeMem :: (Storable a, UsesMemory env m) => Word16 -> a -> ReaderT env m ()
writeMem addr value = do
  Memory {..} <- asks forMemory
  case addr .&. 0xE000 of
    0x0000 -> pure ()
    0x2000 -> pure ()
    0x4000 -> pure ()
    0x6000 -> pure ()
    0x8000 -> liftIO (pokeByteOff (vram videoBuffer) ramOffset value)
    0xA000 -> liftIO (withForeignPtr memRam $ \ptr -> pokeByteOff ptr ramOffset value)
    0xC000 -> liftIO (withForeignPtr memRam $ \ptr -> pokeByteOff ptr ramOffset value)
    0xE000 -> liftIO (withForeignPtr memRam $ \ptr -> pokeByteOff ptr ramOffset value)
    x      -> error ("Impossible coarse read address" ++ show x)
  where ramOffset = fromIntegral addr - 0x8000

-- | Read a chunk of memory.
{-# INLINABLE readChunk #-}
readChunk :: (UsesMemory env m) => Word16 -> Int -> ReaderT env m B.ByteString
readChunk base len = B.pack <$> traverse readByte [base .. base + fromIntegral len]
