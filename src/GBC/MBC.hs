module GBC.MBC
  ( module GBC.MBC.Interface
  , nullMBC
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
