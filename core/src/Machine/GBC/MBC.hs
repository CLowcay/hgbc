module Machine.GBC.MBC
  ( module Machine.GBC.MBC.Interface
  , nullMBC
  , nullRTC
  , volatileRAM
  , savedRAM
  , RTC.savedRTC
  , mbc1
  , mbc3
  , mbc5
  )
where

import           Machine.GBC.MBC.Interface
import           Machine.GBC.MBC.MBC1
import           Machine.GBC.MBC.MBC3
import           Machine.GBC.MBC.MBC5
import           System.IO.MMap
import qualified Data.Vector.Storable.Mutable  as VSM
import qualified Machine.GBC.MBC.RTC           as RTC

-- | Simulate a cartridge with no memory bank controller.
nullMBC :: IO MBC
nullMBC = do
  ram <- VSM.new 0x2000
  pure MBC { bankOffset        = pure 0x4000
           , ramBankOffset     = pure 0
           , ramGate           = pure False
           , writeROM          = \_ _ -> pure ()
           , readRAM           = VSM.unsafeRead ram . fromIntegral
           , readRAMBankOffset = \_ -> VSM.unsafeRead ram . fromIntegral
           , writeRAM          = \address value -> VSM.unsafeWrite ram (fromIntegral address) value
           , sliceRAM          = \address size -> pure (VSM.slice (fromIntegral address) size ram)
           }

nullRTC :: RTC
nullRTC = RTC { readRTC = pure . const 0, writeRTC = \_ _ -> pure (), latchRTC = const (pure ()) }

-- | Allocate volatile RAM.
volatileRAM :: RAMAllocator
volatileRAM = VSM.new

-- | Allocate non-volatile RAM backed by a file.
savedRAM :: String -> RAMAllocator
savedRAM filename size = do
  (ptr, offset, _) <- mmapFileForeignPtr filename ReadWriteEx (Just (0, size))
  pure (VSM.unsafeFromForeignPtr ptr offset size)
