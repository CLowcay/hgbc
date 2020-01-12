module GBC.MBC
  ( module GBC.MBC.Interface
  , nullMBC
  , nullRTC
  , volatileRAM
  , savedRAM
  , savedRTC
  , mbc1
  , mbc3
  , mbc5
  )
where

import           GBC.MBC.Interface
import           GBC.MBC.MBC1
import           GBC.MBC.MBC3
import           GBC.MBC.MBC5
import           System.FilePath
import           System.IO.MMap
import qualified GBC.MBC.RTC                   as RTC
import qualified Data.Vector.Storable.Mutable  as VSM

-- | Simulate a cartridge with no memory bank controller.
nullMBC :: IO MBC
nullMBC = do
  ram <- VSM.new 0x2000
  pure MBC { bankOffset   = pure 0x4000
           , writeROM     = \_ _ -> pure ()
           , readRAM      = \_ address -> VSM.read ram (fromIntegral address)
           , writeRAM     = \_ address value -> VSM.write ram (fromIntegral address) value
           , sliceRAM     = \_ address size -> pure (VSM.slice (fromIntegral address) size ram)
           , mbcRegisters = pure []
           }

nullRTC :: RTC
nullRTC = RTC { readRTC = pure . const 0, writeRTC = \_ _ -> pure (), latchRTC = const (pure ()) }

-- | Allocate volatile RAM.
volatileRAM :: RAMAllocator
volatileRAM = VSM.new

-- | Allocate non-volatile RAM backed by a file.
savedRAM :: String -> RAMAllocator
savedRAM filename size = do
  (ptr, offset, _) <- mmapFileForeignPtr (filename -<.> "sav") ReadWriteEx (Just (0, size))
  pure (VSM.unsafeFromForeignPtr ptr offset size)

savedRTC :: String -> IO RTC
savedRTC filename = RTC.savedRTC (filename -<.> "rtc")
