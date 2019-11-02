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
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B

-- | Simulate a cartridge with no memory bank controller.
nullMBC :: B.ByteString -> IO MBC
nullMBC romData = do
    ram <- mallocForeignPtrBytes 0x2000
    pure MBC
        { readROMLow         = \address -> pure (romData `B.unsafeIndex` fromIntegral address)
        , withROMLowPointer  =
            \address action ->
                B.unsafeUseAsCString romData (action . (`plusPtr` fromIntegral address))
        , readROMHigh = \address -> pure (romData `B.unsafeIndex` (fromIntegral address + 0x4000))
        , withROMHighPointer = \address action -> B.unsafeUseAsCString
                                   romData
                                   (action . (`plusPtr` (fromIntegral address + 0x4000)))
        , writeROM           = \_ _ -> pure ()
        , readRAM            = \_ address ->
                                   withForeignPtr ram $ \ptr -> peekElemOff ptr (fromIntegral address)
        , writeRAM           = \_ address value -> withForeignPtr ram
                                   $ \ptr -> pokeElemOff ptr (fromIntegral address) value
        , withRAMPointer     = \_ address action ->
                                   withForeignPtr ram (action . (`plusPtr` fromIntegral address))
        , mbcRegisters       = pure []
        }
