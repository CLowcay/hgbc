{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GBC.Memory where

import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.ROM
import qualified Data.ByteString               as B

data Memory = Memory {
    memRom :: B.ByteString
  , memRam :: ForeignPtr Word8
}

initMemory :: ROM -> IO Memory
initMemory (ROM rom) = Memory rom <$> mallocForeignPtrArray 0x7FFF

readByte :: Memory -> Word16 -> IO Word8
readByte Memory {..} addr = if addr < 0x8000
  then pure $ memRom `B.index` fromIntegral addr
  else withForeignPtr memRam $ flip peekByteOff (fromIntegral addr - 0x8000)

writeMem :: Storable a => Memory -> Word16 -> a -> IO ()
writeMem Memory {..} addr value = if addr < 0x8000
  then pure ()
  else withForeignPtr memRam
    $ \ptr -> pokeByteOff ptr (fromIntegral addr - 0x8000) value

readChunk :: Memory -> Word16 -> Int -> IO B.ByteString
readChunk Memory {..} base len = (<>) <$> romData <*> ramData
 where
  romData = pure $ if base >= 0x8000
    then ""
    else
      B.take (len `min` fromIntegral (0x8000 - base))
      . B.drop (fromIntegral base)
      $ memRom
  ramData = if fromIntegral base + len <= 0x8000
    then pure ""
    else
      let base' = fromIntegral base `max` 0x8000
          len'  = len + fromIntegral base - base'
      in  withForeignPtr memRam
            $ \ptr -> B.packCStringLen (castPtr $ ptr `plusPtr` base', len')
