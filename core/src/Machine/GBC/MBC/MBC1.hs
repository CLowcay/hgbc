{-# LANGUAGE TypeFamilies #-}
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

mbc1 :: Int -> Int -> Bool -> RAMAllocator -> IO MBC
mbc1 bankMask ramMask multicart ramAllocator = do
  bank1               <- newIORef 1
  bank2               <- newIORef 0
  enableRAM           <- newIORef False
  bankMode            <- newIORef False
  ram                 <- ramAllocator 0x8000

  cachedROMOffsetHigh <- newIORef 0x4000
  cachedROMOffsetLow  <- newIORef 0
  cachedRAMOffset     <- newIORef 0

  let highShift      = if multicart then 4 else 5
  let lowMask        = if multicart then 0xF else 0x1F

  let lowBankOffset  = readIORef cachedROMOffsetLow
  let highBankOffset = readIORef cachedROMOffsetHigh
  let ramBankOffset  = readIORef cachedRAMOffset
  let ramGate        = readIORef enableRAM

  let updateROMOffset = do
        mode1 <- readIORef bankMode
        low   <- readIORef bank1
        high  <- readIORef bank2
        writeIORef cachedROMOffsetHigh ((((high .<<. highShift) .|. low) .&. bankMask) .<<. 14)
        if mode1
          then writeIORef cachedROMOffsetLow (((high .<<. highShift) .&. bankMask) .<<. 14)
          else writeIORef cachedROMOffsetLow 0

  let updateRAMOffset = do
        mode1 <- readIORef bankMode
        bank  <- if mode1 then readIORef bank2 else pure 0
        writeIORef cachedRAMOffset ((bank .&. ramMask) .<<. 13)

  let writeROM address value
        | address < 0x2000
        = writeIORef enableRAM (value .&. 0x0F == 0x0A)
        | address < 0x4000
        = let low = (fromIntegral value .&. 0x1F)
          in  do
                writeIORef bank1 (if low == 0 then 1 else low .&. lowMask)
                updateROMOffset
        | address < 0x6000
        = do
          writeIORef bank2 (fromIntegral value .&. 0x3)
          updateROMOffset
          updateRAMOffset
        | otherwise
        = do
          writeIORef bankMode (value `testBit` 0)
          updateROMOffset
          updateRAMOffset
  let readRAM address = do
        enabled <- readIORef enableRAM
        if not enabled
          then pure 0xFF
          else do
            offset <- readIORef cachedRAMOffset
            VSM.unsafeRead ram (offset + fromIntegral address)
  let readRAMBankOffset offset address = VSM.unsafeRead ram (offset + fromIntegral address)
  let writeRAM address value = do
        enabled <- readIORef enableRAM
        when enabled $ do
          offset <- readIORef cachedRAMOffset
          VSM.unsafeWrite ram (offset + fromIntegral address) value
  pure MBC { .. }
