{-# LANGUAGE RecordWildCards #-}
module GBC.MBC.MBC5
  ( mbc5
  )
where

import           Common
import           Control.Exception              ( throwIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.IORef
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Errors
import           GBC.MBC.Interface

mbc5 :: Int -> Int -> RAMAllocator -> IO MBC
mbc5 bankMask ramMask ramAllocator = do
  ramG                <- newIORef False
  romB0               <- newIORef 1
  romB1               <- newIORef 0
  ramB                <- newIORef 0
  (ram, ramPtrOffset) <- ramAllocator 0x40000

  cachedROMOffset     <- newIORef 0x4000

  let
    updateROMOffset = do
      low  <- readIORef romB0
      high <- readIORef romB1
      writeIORef cachedROMOffset
                 ((((high `unsafeShiftL` 8) .|. low) .&. bankMask) `unsafeShiftL` 14)

  let bankOffset = readIORef cachedROMOffset

  let getRAMOffset = do
        bank <- readIORef ramB
        pure ((bank .&. ramMask) `unsafeShiftL` 13)

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
        withForeignPtr ram
          $ \ptr -> peekElemOff (ptr `plusPtr` ramPtrOffset) (offset + fromIntegral address)
  let
    writeRAM check address value = do
      when check $ liftIO $ do
        enabled <- readIORef ramG
        unless enabled (throwIO (InvalidWrite (address + 0xA000)))
      offset <- getRAMOffset
      withForeignPtr ram
        $ \ptr -> pokeElemOff (ptr `plusPtr` ramPtrOffset) (offset + fromIntegral address) value
  let withRAMPointer check address action = do
        when check $ liftIO $ do
          enabled <- readIORef ramG
          unless enabled (throwIO (InvalidWrite (address + 0xA000)))
        offset <- getRAMOffset
        withForeignPtr ram (action . (`plusPtr` (ramPtrOffset + offset + fromIntegral address)))
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
