{-# LANGUAGE RankNTypes #-}

-- | Common interface for Memory Bank Controllers.
module GBC.MBC.Interface
  ( MBC(..)
  , RAMAllocator
  , volatileRAM
  , savedRAM
  )
where

import           Common
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.FilePath
import           System.IO.MMap

type RAMAllocator = Int -> IO (ForeignPtr Word8, Int)

-- | Allocate volatile RAM.
volatileRAM :: RAMAllocator
volatileRAM size = do
  ptr <- mallocForeignPtrBytes size
  pure (ptr, 0)

-- | Allocate non-volatile RAM backed by a file.
savedRAM :: String -> RAMAllocator
savedRAM filename size = do
  (ptr, offset, _) <- mmapFileForeignPtr (filename -<.> "sav") ReadWriteEx (Just (0, size))
  pure (ptr, offset)

-- | A memory bank controller.
data MBC = MBC {
    readROMLow :: Word16 -> IO Word8
  , readROMHigh :: Word16 -> IO Word8
  , writeROM :: Word16 -> Word8 -> IO ()
  , withROMLowPointer :: forall a. Word16 -> (Ptr Word8 -> IO a) -> IO a
  , withROMHighPointer :: forall a. Word16 -> (Ptr Word8 -> IO a) -> IO a
  , readRAM :: Bool -> Word16 -> IO Word8
  , writeRAM :: Bool -> Word16 -> Word8 -> IO ()
  , withRAMPointer :: forall a. Bool -> Word16 -> (Ptr Word8 -> IO a) -> IO a
  , mbcRegisters :: IO [RegisterInfo]
}
