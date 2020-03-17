{-# LANGUAGE RecordWildCards #-}
module Machine.GBC.MBC.MBC5
  ( mbc5
  )
where

import           Control.Exception              ( throwIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.IORef
import           Machine.GBC.Errors
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

  let bankOffset = readIORef cachedROMOffset

  let getRAMOffset = do
        bank <- readIORef ramB
        pure ((bank .&. ramMask) .<<. 13)

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
  let readRAM check address = do
        when check $ liftIO $ do
          enabled <- readIORef ramG
          unless enabled (throwIO (InvalidRead (address + 0xA000)))
        offset <- getRAMOffset
        VSM.unsafeRead ram (offset + fromIntegral address)
  let writeRAM check address value = do
        when check $ liftIO $ do
          enabled <- readIORef ramG
          unless enabled (throwIO (InvalidWrite (address + 0xA000)))
        offset <- getRAMOffset
        VSM.unsafeWrite ram (offset + fromIntegral address) value
  let sliceRAM check address size = do
        when check $ liftIO $ do
          enabled <- readIORef ramG
          unless enabled (throwIO (InvalidWrite (address + 0xA000)))
        offset <- getRAMOffset
        pure (VSM.unsafeSlice (offset + fromIntegral address) size ram)
  let mbcRegisters = do
        g    <- readIORef ramG
        b0   <- readIORef romB0
        b1   <- readIORef romB1
        ramb <- readIORef ramB
        pure
          [ RegisterInfo 0      "ROMG"  (if g then 0x0A else 0) [("RAM enabled ", show g)]
          , RegisterInfo 0x2000 "ROMB0" (fromIntegral b0)       []
          , RegisterInfo 0x3000 "ROMB1" (fromIntegral b1)       []
          , RegisterInfo 0x4000 "RAMB"  (fromIntegral ramb)     []
          ]
  pure MBC { .. }
