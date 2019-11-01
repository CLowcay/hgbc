{-# LANGUAGE RankNTypes #-}

-- | Common interface for Memory Bank Controllers.
module GBC.MBC.Interface
    ( MBC(..)
    )
where

import           Common
import           Data.Word
import           Foreign.Ptr

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
