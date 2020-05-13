{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Machine.GBC.MBC.MBC5
  ( mbc5
  )
where

import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Machine.GBC.MBC.Interface
import           Machine.GBC.Util
import qualified Data.Vector.Storable.Mutable  as VSM

mbc5 :: Int -> Int -> RAMAllocator -> IO MBC
mbc5 bankMask ramMask ramAllocator = do
  ramG            <- newIORef False
  romB0           <- newIORef 1
  romB1           <- newIORef 0
  ramB            <- newIORef 0
  ram             <- ramAllocator 0x40000

  cachedROMOffset <- newIORef 0x4000

  let updateROMOffset = do
        low  <- readIORef romB0
        high <- readIORef romB1
        writeIORef cachedROMOffset ((((high .<<. 8) .|. low) .&. bankMask) .<<. 14)

  let lowBankOffset  = pure 0
  let highBankOffset = readIORef cachedROMOffset

  let ramBankOffset = do
        bank <- readIORef ramB
        pure ((bank .&. ramMask) .<<. 13)

  let ramGate = readIORef ramG

  let writeROM address value
        | address < 0x2000 = writeIORef ramG (value .&. 0x0F == 0x0A)
        | address < 0x3000 = do
          writeIORef romB0 (fromIntegral value)
          updateROMOffset
        | address < 0x4000 = do
          writeIORef romB1 (fromIntegral value .&. 1)
          updateROMOffset
        | address < 0x6000 = writeIORef ramB (fromIntegral value .&. 0xF)
        | otherwise = pure ()
  let readRAM address = do
        enabled <- readIORef ramG
        if not enabled
          then pure 0xFF
          else do
            offset <- ramBankOffset
            VSM.unsafeRead ram (offset + fromIntegral address)
  let readRAMBankOffset offset address = VSM.unsafeRead ram (offset + fromIntegral address)
  let writeRAM address value = do
        enabled <- readIORef ramG
        when enabled $ do
          offset <- ramBankOffset
          VSM.unsafeWrite ram (offset + fromIntegral address) value
  pure MBC { .. }
