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
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Errors
import           GBC.MBC.Interface
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as B

mbc1 :: B.ByteString -> IO MBC
mbc1 romData = do
    romOffset <- newIORef 1
    ramOffset <- newIORef 0
    enableRAM <- newIORef False
    ramSelect <- newIORef False
    ram       <- mallocForeignPtrBytes 0x8000 :: IO (ForeignPtr Word8)

    let getROMOffset = do
            noHighROM <- readIORef ramSelect
            low       <- readIORef romOffset
            if noHighROM
                then pure (low `unsafeShiftL` 14)
                else do
                    high <- readIORef ramOffset
                    pure (((high `unsafeShiftL` 5) .|. low) `unsafeShiftL` 14)
    let getRAMOffset = do
            ramBanking <- readIORef ramSelect
            if ramBanking
                then do
                    offset <- readIORef ramOffset
                    pure (offset `unsafeShiftL` 13)
                else pure 0

    let readROMLow address = pure (romData `B.unsafeIndex` fromIntegral address)
    let readROMHigh address = do
            offset <- getROMOffset
            pure (romData `B.unsafeIndex` (offset + fromIntegral address))
    let
        writeROM address value
            | address < 0x2000
            = writeIORef enableRAM (value .&. 0x0F == 0x0A)
            | address < 0x4000
            = let low = (fromIntegral value .&. 0x1F)
              in  writeIORef romOffset (if low == 0 then 1 else low)
            | address < 0x6000
            = writeIORef ramOffset (fromIntegral value .&. 0x3)
            | otherwise
            = writeIORef ramSelect (value /= 0)
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
