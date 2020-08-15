module Machine.GBC.MBC
  ( module Machine.GBC.MBC.Interface,
    nullMBC,
    nullRTC,
    volatileRAM,
    savedRAM,
    RTC.savedRTC,
    mbc1,
    mbc2,
    mbc3,
    mbc5,
  )
where

import qualified Data.Vector.Storable.Mutable as VSM
import Machine.GBC.MBC.Interface
import Machine.GBC.MBC.MBC1
import Machine.GBC.MBC.MBC2
import Machine.GBC.MBC.MBC3
import Machine.GBC.MBC.MBC5
import qualified Machine.GBC.MBC.RTC as RTC
import System.IO.MMap

-- | Simulate a cartridge with no memory bank controller.
nullMBC :: IO MBC
nullMBC = do
  ram <- VSM.new 0x2000
  pure
    MBC
      { lowBankOffset = pure 0,
        highBankOffset = pure 0x4000,
        ramBankOffset = pure 0,
        ramGate = pure False,
        writeROM = \_ _ -> pure (),
        readRAM = VSM.unsafeRead ram . fromIntegral,
        readRAMBankOffset = \_ -> VSM.unsafeRead ram . fromIntegral,
        writeRAM = \address value -> VSM.unsafeWrite ram (fromIntegral address) value
      }

nullRTC :: RTC
nullRTC = RTC {readRTC = pure . const 0, writeRTC = \_ _ -> pure (), latchRTC = const (pure ())}

-- | Allocate volatile RAM.
volatileRAM :: RAMAllocator
volatileRAM = VSM.new

-- | Allocate non-volatile RAM backed by a file.
savedRAM :: String -> RAMAllocator
savedRAM filename size = do
  (ptr, offset, _) <- mmapFileForeignPtr filename ReadWriteEx (Just (0, size))
  pure (VSM.unsafeFromForeignPtr ptr offset size)
