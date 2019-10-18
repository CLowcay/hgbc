{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Memory
  ( Memory
  , HasMemory(..)
  , UsesMemory
  , initMemory
  , getROMHeader
  , dma
  , readByte
  , writeMem
  , readChunk
  , withVRAMPointer
  , withBGPointer
  , withOAMPointer
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.ROM
import qualified Data.ByteString               as B

data Memory = Memory {
    memRom :: B.ByteString
  , memRam :: ForeignPtr Word8
}

class HasMemory env where
  forMemory :: env -> Memory

instance HasMemory Memory where
  {-# INLINE forMemory #-}
  forMemory = id

-- | Constraints for monads that can access memory.
type UsesMemory env m = (MonadIO m, HasMemory env)

-- | The initial memory state.
initMemory :: ROM -> IO Memory
initMemory (ROM rom) = Memory rom <$> mallocForeignPtrArray 0x7FFF

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = extractHeader $ ROM memRom

-- | Perform an action that requires a pointer to VRAM (0x8000).
withVRAMPointer :: UsesMemory env m => (Ptr Word8 -> IO a) -> ReaderT env m a
withVRAMPointer action = do
  memory <- asks forMemory
  liftIO (withForeignPtr (memRam memory) action)

-- | Perform an action that requires a pointer to BG memory (0x9800).
withBGPointer :: UsesMemory env m => (Ptr Word8 -> IO a) -> ReaderT env m a
withBGPointer action = do
  memory <- asks forMemory
  liftIO (withForeignPtr (memRam memory) $ \ptr -> action (ptr `plusPtr` 0x1800))

-- | Perform an action that requires a pointer to OAM memory (0xFE00).
withOAMPointer :: UsesMemory env m => (Ptr Word8 -> IO a) -> ReaderT env m a
withOAMPointer action = do
  memory <- asks forMemory
  liftIO (withForeignPtr (memRam memory) $ \ptr -> action (ptr `plusPtr` 0x7E00))

-- | Perform a direct memory transfer.
dma :: UsesMemory env m => Word16 -> Word16 -> Int -> ReaderT env m ()
dma source destination size =
  if source < 0x8000 || destination < 0x8000 || size > fromIntegral (0xFFFF - destination)
    then error $ "DMA out of bounds " ++ formatHex source ++ " " ++ formatHex destination
    else do
      Memory {..} <- asks forMemory
      let sourceOffset      = (fromIntegral source) - 0x8000
      let destinationOffset = (fromIntegral destination) - 0x8000
      liftIO $ withForeignPtr memRam $ \ram -> moveArray
        (ram `plusPtr` sourceOffset :: Ptr Word8)
        (ram `plusPtr` destinationOffset :: Ptr Word8)
        size

-- | Read a byte from memory.
{-# INLINE readByte #-}
readByte :: UsesMemory env m => Word16 -> ReaderT env m Word8
readByte addr = do
  Memory {..} <- asks forMemory
  if addr < 0x8000
    then pure $ memRom `B.index` fromIntegral addr
    else liftIO $ withForeignPtr memRam $ flip peekByteOff (fromIntegral addr - 0x8000)

-- | Write to memory.
{-# INLINE writeMem #-}
writeMem :: (Storable a, UsesMemory env m) => Word16 -> a -> ReaderT env m ()
writeMem addr value = do
  Memory {..} <- asks forMemory
  if addr < 0x8000
    then pure ()
    else liftIO $ withForeignPtr memRam $ \ptr -> pokeByteOff ptr (fromIntegral addr - 0x8000) value

-- | Read a chunk of memory.
{-# INLINABLE readChunk #-}
readChunk :: (UsesMemory env m) => Word16 -> Int -> ReaderT env m B.ByteString
readChunk base len = do
  Memory {..} <- asks forMemory
  let romData = pure $ if base >= 0x8000
        then ""
        else B.take (len `min` fromIntegral (0x8000 - base)) . B.drop (fromIntegral base) $ memRom
  let ramData = if fromIntegral base + len <= 0x8000
        then pure ""
        else
          let base' = fromIntegral base `max` 0x8000
              len'  = len + fromIntegral base - base'
          in  withForeignPtr memRam
                $ \ptr -> B.packCStringLen (castPtr $ ptr `plusPtr` (base' - 0x8000), len')
  liftIO $ (<>) <$> romData <*> ramData
