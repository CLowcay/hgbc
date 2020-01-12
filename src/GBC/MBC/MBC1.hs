{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module GBC.MBC.MBC1
  ( mbc1
  )
where

import           Common
import           Control.Exception              ( throwIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.IORef
import           GBC.Errors
import           GBC.MBC.Interface
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
  let readRAM check address = do
        when check $ liftIO $ do
          enabled <- readIORef enableRAM
          unless enabled (throwIO (InvalidRead (address + 0xA000)))
        offset <- readIORef cachedRAMOffset
        VSM.read ram (offset + fromIntegral address)
  let writeRAM check address value = do
        when check $ do
          enabled <- readIORef enableRAM
          unless enabled (throwIO (InvalidWrite (address + 0xA000)))
        offset <- readIORef cachedRAMOffset
        VSM.write ram (offset + fromIntegral address) value
  let sliceRAM check address size = do
        when check $ do
          enabled <- readIORef enableRAM
          unless enabled (throwIO (InvalidAccess (address + 0xA000)))
        offset <- readIORef cachedRAMOffset
        pure (VSM.slice (offset + fromIntegral address) size ram)
  let mbcRegisters = do
        r1 <- readIORef romOffset
        r2 <- readIORef ramOffset
        r0 <- readIORef enableRAM
        r3 <- readIORef ramSelect
        pure
          [ RegisterInfo 0      "R0" (if r0 then 0x0A else 0) [("RAM enabled ", show r0)]
          , RegisterInfo 0x2000 "R1" (fromIntegral r1)        []
          , RegisterInfo 0x4000 "R2" (fromIntegral r2)        []
          , RegisterInfo 0x6000
                         "R3"
                         (if r3 then 1 else 0)
                         [("R2 is", if r3 then "RAM bank" else "ROM bank high bits")]
          ]
  pure MBC { .. }
