{-# LANGUAGE RankNTypes #-}

-- | Common interface for Memory Bank Controllers.
module Machine.GBC.MBC.Interface
  ( MBC(..)
  , RTC(..)
  , RAMAllocator
  )
where

import           Data.Word
import           Machine.GBC.Util
import qualified Data.Vector.Storable.Mutable  as VSM

type RAMAllocator = Int -> IO (VSM.IOVector Word8)

-- | A memory bank controller.
data MBC = MBC {
    bankOffset     :: IO Int
  , writeROM       :: Word16 -> Word8 -> IO ()
  , readRAM        :: Bool -> Word16 -> IO Word8
  , writeRAM       :: Bool -> Word16 -> Word8 -> IO ()
  , sliceRAM       :: Bool -> Word16 -> Int -> IO (VSM.IOVector Word8)
  , mbcRegisters   :: IO [RegisterInfo]
}

data RTC = RTC {
    readRTC  :: Int -> IO Word8
  , writeRTC :: Int -> Word8 -> IO ()
  , latchRTC :: Word8 -> IO ()
}
