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

import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.MBC.Interface
import           GBC.MBC.MBC1
import           GBC.MBC.MBC3
import           GBC.MBC.MBC5
import           System.FilePath
import           System.IO.MMap
import qualified GBC.MBC.RTC                   as RTC

-- | Simulate a cartridge with no memory bank controller.
nullMBC :: IO MBC
nullMBC = do
  ram <- mallocForeignPtrBytes 0x2000
  pure MBC
    { bankOffset     = pure 0x4000
    , writeROM       = \_ _ -> pure ()
    , readRAM = \_ address -> withForeignPtr ram $ \ptr -> peekElemOff ptr (fromIntegral address)
    , writeRAM       = \_ address value ->
                         withForeignPtr ram $ \ptr -> pokeElemOff ptr (fromIntegral address) value
    , withRAMPointer = \_ address action ->
                         withForeignPtr ram (action . (`plusPtr` fromIntegral address))
    , mbcRegisters   = pure []
    }

nullRTC :: RTC
nullRTC = RTC { readRTC = pure . const 0, writeRTC = \_ _ -> pure (), latchRTC = const (pure ()) }

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

savedRTC :: String -> IO RTC
savedRTC filename = RTC.savedRTC (filename -<.> "rtc")
