{-# LANGUAGE RankNTypes #-}

-- | Common interface for Memory Bank Controllers.
module Machine.GBC.MBC.Interface
  ( MBC(..)
  , RTC(..)
  , RTCRegister(..)
  , RAMAllocator
  )
where

import           Data.Word
import qualified Data.Vector.Storable.Mutable  as VSM

type RAMAllocator = Int -> IO (VSM.IOVector Word8)

-- | A memory bank controller.
data MBC = MBC {
    bankOffset     :: !(IO Int)
  , ramBankOffset  :: !(IO Int)
  , ramGate        :: !(IO Bool)
  , writeROM       :: !(Word16 -> Word8 -> IO ())
  , readRAM        :: !(Word16 -> IO Word8)
  , readRAMBankOffset :: !(Int -> Word16 -> IO Word8)
  , writeRAM       :: !(Word16 -> Word8 -> IO ())
  , sliceRAM       :: !(Word16 -> Int -> IO (VSM.IOVector Word8))
}

data RTCRegister = Seconds | Minutes | Hours | DaysLow | DaysHigh deriving (Eq, Ord, Show)

data RTC = RTC {
    readRTC  :: !(RTCRegister -> IO Word8)
  , writeRTC :: !(RTCRegister -> Word8 -> IO ())
  , latchRTC :: !(Word8 -> IO ())
}