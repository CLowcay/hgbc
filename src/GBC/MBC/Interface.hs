{-# LANGUAGE RankNTypes #-}

-- | Common interface for Memory Bank Controllers.
module GBC.MBC.Interface
  ( MBC(..)
  , RTC(..)
  , RAMAllocator
  )
where

import           Common
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr

type RAMAllocator = Int -> IO (ForeignPtr Word8, Int)

-- | A memory bank controller.
data MBC = MBC {
    bankOffset :: IO Int
  , writeROM :: Word16 -> Word8 -> IO ()
  , readRAM :: Bool -> Word16 -> IO Word8
  , writeRAM :: Bool -> Word16 -> Word8 -> IO ()
  , withRAMPointer :: forall a. Bool -> Word16 -> (Ptr Word8 -> IO a) -> IO a
  , mbcRegisters :: IO [RegisterInfo]
}

data RTC = RTC {
    readRTC :: Int -> IO Word8
  , writeRTC :: Int -> Word8 -> IO ()
  , latchRTC :: Word8 -> IO ()
}
