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
  , readByte
  , writeMem
  , readChunk
  )
where

import           Control.Monad.Reader
import           Data.Word
import           Foreign.ForeignPtr
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
  forMemory = id

-- | Constraints for monads that can access memory.
type UsesMemory env m = (MonadIO m, HasMemory env)

-- | The initial memory state.
initMemory :: ROM -> IO Memory
initMemory (ROM rom) = Memory rom <$> mallocForeignPtrArray 0x7FFF

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = extractHeader $ ROM memRom

-- | Read a byte from memory.
readByte :: UsesMemory env m => Word16 -> ReaderT env m Word8
readByte addr = do
  Memory {..} <- asks forMemory
  if addr < 0x8000
    then pure $ memRom `B.index` fromIntegral addr
    else liftIO $ withForeignPtr memRam $ flip peekByteOff (fromIntegral addr - 0x8000)

-- | Write to memory.
writeMem :: (Storable a, UsesMemory env m) => Word16 -> a -> ReaderT env m ()
writeMem addr value = do
  Memory {..} <- asks forMemory
  if addr < 0x8000
    then pure ()
    else liftIO $ withForeignPtr memRam $ \ptr -> pokeByteOff ptr (fromIntegral addr - 0x8000) value

-- | Read a chunk of memory.
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
