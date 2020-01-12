{-# LANGUAGE RecordWildCards #-}
module GBC.MBC.MBC3
  ( mbc3
  )
where

import           Common
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.IORef
import           GBC.Errors
import           GBC.MBC.Interface
import qualified Data.Vector.Storable.Mutable  as VSM

mbc3 :: Int -> Int -> RAMAllocator -> RTC -> IO MBC
mbc3 bankMask ramMask ramAllocator rtc = do
  romOffset <- newIORef 1
  register2 <- newIORef 0
  enableRAM <- newIORef False
  ram       <- ramAllocator 0x8000

  let bankOffset = do
        offset <- readIORef romOffset
        pure ((offset .&. bankMask) .<<. 14)
  let getRAMOffset r2 = (r2 .&. ramMask) .<<. 13

  let
    writeROM address value
      | address < 0x2000
      = writeIORef enableRAM (value .&. 0x0F == 0x0A)
      | address < 0x4000
      = let offset = (fromIntegral value .&. 0x7F)
        in  writeIORef romOffset (if offset == 0 then 1 else offset)
      | address < 0x6000
      = writeIORef register2
                   (fromIntegral (if value >= 8 && value <= 0xC then value else value .&. 3))
      | otherwise
      = latchRTC rtc value

  let readRAM check address = do
        when check $ liftIO $ do
          enabled <- readIORef enableRAM
          unless enabled (throwIO (InvalidRead (address + 0xA000)))
        r2 <- readIORef register2
        if r2 >= 8 && r2 <= 0xC
          then readRTC rtc r2
          else VSM.read ram (getRAMOffset r2 + fromIntegral address)
  let writeRAM check address value = do
        when check $ liftIO $ do
          enabled <- readIORef enableRAM
          unless enabled (throwIO (InvalidWrite (address + 0xA000)))
        r2 <- readIORef register2
        if r2 >= 8 && r2 <= 0xC
          then writeRTC rtc r2 value
          else VSM.write ram (getRAMOffset r2 + fromIntegral address) value
  let sliceRAM check address size = do
        when check $ liftIO $ do
          enabled <- readIORef enableRAM
          unless enabled (throwIO (InvalidAccess (address + 0xA000)))
        offset <- getRAMOffset <$> readIORef register2
        pure (VSM.slice (offset + fromIntegral address) size ram)
  let mbcRegisters = do
        r1 <- readIORef romOffset
        r2 <- readIORef register2
        r0 <- readIORef enableRAM
        pure
          [ RegisterInfo 0      "R0" (if r0 then 0x0A else 0) [("RAM enabled ", show r0)]
          , RegisterInfo 0x2000 "R1" (fromIntegral r1)        []
          , RegisterInfo 0x4000 "R2" (fromIntegral r2)        []
          ]
  pure MBC { .. }
