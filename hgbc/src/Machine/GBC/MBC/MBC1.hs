{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Machine.GBC.MBC.MBC1
  ( mbc1
  )
where

import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Machine.GBC.MBC.Interface
import           Machine.GBC.Util
import qualified Data.Vector.Storable.Mutable  as VSM

mbc1 :: Int -> Int -> RAMAllocator -> IO MBC
mbc1 bankMask ramMask ramAllocator = do
  romOffset       <- newIORef 1
  ramOffset       <- newIORef 0
  enableRAM       <- newIORef False
  ramSelect       <- newIORef False
  ram             <- ramAllocator 0x8000

  cachedROMOffset <- newIORef 0x4000
  cachedRAMOffset <- newIORef 0

  let bankOffset = readIORef cachedROMOffset

  let updateROMOffset = do
        noHighROM <- readIORef ramSelect
        low       <- readIORef romOffset
        bank      <- if noHighROM
          then pure low
          else do
            high <- readIORef ramOffset
            pure ((high .<<. 5) .|. low)
        writeIORef cachedROMOffset ((bank .&. bankMask) .<<. 14)

  let updateRAMOffset = do
        ramBanking <- readIORef ramSelect
        bank       <- if ramBanking then readIORef ramOffset else pure 0
        writeIORef cachedRAMOffset ((bank .&. ramMask) .<<. 13)

  let writeROM address value
        | address < 0x2000
        = writeIORef enableRAM (value .&. 0x0F == 0x0A)
        | address < 0x4000
        = let low = (fromIntegral value .&. 0x1F)
          in  do
                writeIORef romOffset (if low == 0 then 1 else low)
                updateROMOffset
        | address < 0x6000
        = do
          writeIORef ramOffset (fromIntegral value .&. 0x3)
          updateROMOffset
          updateRAMOffset
        | otherwise
        = do
          writeIORef ramSelect (value /= 0)
          updateROMOffset
          updateRAMOffset
  let readRAM address = do
        enabled <- readIORef enableRAM
        if not enabled
          then pure 0xFF
          else do
            offset <- readIORef cachedRAMOffset
            VSM.unsafeRead ram (offset + fromIntegral address)
  let writeRAM address value = do
        enabled <- readIORef enableRAM
        when enabled $ do
          offset <- readIORef cachedRAMOffset
          VSM.unsafeWrite ram (offset + fromIntegral address) value
  let sliceRAM address size = do
        enabled <- readIORef enableRAM
        if not enabled
          then VSM.replicate size 0xFF
          else do
            offset <- readIORef cachedRAMOffset
            pure (VSM.unsafeSlice (offset + fromIntegral address) size ram)
  pure MBC { .. }
