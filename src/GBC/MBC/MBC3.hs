{-# LANGUAGE RecordWildCards #-}
module GBC.MBC.MBC3
    ( mbc3
    )
where

import           Common
import           Control.Exception              ( throwIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Errors
import           GBC.MBC.Interface
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B

mbc3 :: B.ByteString -> IO MBC
mbc3 romData = do
    romOffset <- newIORef 1
    ramOffset <- newIORef 0
    enableRAM <- newIORef False
    ram       <- mallocForeignPtrBytes 0x8000 :: IO (ForeignPtr Word8)

    let getROMOffset = do
            offset <- readIORef romOffset
            pure (offset `unsafeShiftL` 14)
    let getRAMOffset = do
            offset <- readIORef ramOffset
            pure (offset `unsafeShiftL` 13)

    let readROMLow address = pure (romData `B.unsafeIndex` fromIntegral address)
    let readROMHigh address = do
            offset <- getROMOffset
            pure (romData `B.unsafeIndex` (offset + fromIntegral address))
    let writeROM address value
            | address < 0x2000
            = writeIORef enableRAM (value .&. 0x0F == 0x0A)
            | address < 0x4000
            = let offset = (fromIntegral value .&. 0x7F)
              in  writeIORef romOffset (if offset == 0 then 1 else offset)
            | address < 0x6000
            = writeIORef ramOffset (fromIntegral value .&. 0x3)
            | otherwise
            = pure () -- TODO: latch RTC
    let withROMLowPointer address action =
            B.unsafeUseAsCString romData (action . (`plusPtr` fromIntegral address))
    let withROMHighPointer address action = do
            offset <- getROMOffset
            B.unsafeUseAsCString romData (action . (`plusPtr` (offset + fromIntegral address)))
    let readRAM check address = do
            when check $ liftIO $ do
                enabled <- readIORef enableRAM
                unless enabled (throwIO (InvalidRead (address + 0xA000)))
            offset <- getRAMOffset
            withForeignPtr ram $ \ptr -> peekElemOff ptr (offset + fromIntegral address)
    let writeRAM check address value = do
            when check $ liftIO $ do
                enabled <- readIORef enableRAM
                unless enabled (throwIO (InvalidWrite (address + 0xA000)))
            offset <- getRAMOffset
            withForeignPtr ram $ \ptr -> pokeElemOff ptr (offset + fromIntegral address) value
    let withRAMPointer check address action = do
            when check $ liftIO $ do
                enabled <- readIORef enableRAM
                unless enabled (throwIO (InvalidAccess (address + 0xA000)))
            offset <- getRAMOffset
            withForeignPtr ram (action . (`plusPtr` (offset + fromIntegral address)))
    let
        mbcRegisters = do
            r1 <- readIORef romOffset
            r2 <- readIORef ramOffset
            r0 <- readIORef enableRAM
            pure
                [ RegisterInfo 0      "R0" (if r0 then 0x0A else 0) [("RAM enabled ", show r0)]
                , RegisterInfo 0x2000 "R1" (fromIntegral r1)        []
                , RegisterInfo 0x4000 "R2" (fromIntegral r2)        []
                ]
    pure MBC { .. }
