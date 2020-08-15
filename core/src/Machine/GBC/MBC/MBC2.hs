{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.MBC.MBC2
  ( mbc2,
  )
where

import Control.Monad
import Data.Bits
import Data.IORef
import qualified Data.Vector.Storable.Mutable as VSM
import Machine.GBC.MBC.Interface
import Machine.GBC.Util

mbc2 :: Int -> RAMAllocator -> IO MBC
mbc2 bankMask ramAllocator = do
  ramG <- newIORef False
  romB <- newIORef 1
  ram <- ramAllocator 512

  cachedROMOffset <- newIORef 0x4000

  let updateROMOffset = do
        low <- readIORef romB
        writeIORef cachedROMOffset ((low .&. bankMask) .<<. 14)

  let lowBankOffset = pure 0
  let highBankOffset = readIORef cachedROMOffset
  let ramBankOffset = pure 0
  let ramGate = readIORef ramG

  let writeROM address value
        | address < 0x4000 =
          if address .&. 0x0100 == 0
            then writeIORef ramG (value .&. 0x0F == 0x0A)
            else do
              let low = fromIntegral value .&. 0x0F
              writeIORef romB (if low == 0 then 1 else low)
              updateROMOffset
        | otherwise = pure ()
  let readRAM address = do
        enabled <- readIORef ramG
        if not enabled then pure 0xFF else VSM.unsafeRead ram (fromIntegral address .&. 0x01FF)
  let readRAMBankOffset _ address = VSM.unsafeRead ram (fromIntegral address .&. 0x01FF)
  let writeRAM address value = do
        enabled <- readIORef ramG
        when enabled $ VSM.unsafeWrite ram (fromIntegral address .&. 0x01FF) (value .|. 0xF0)
  pure MBC {..}
