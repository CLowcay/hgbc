{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.MBC.MBC3
  ( mbc3,
  )
where

import Control.Monad (when)
import Data.Bits (Bits (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Vector.Storable.Mutable as VSM
import Machine.GBC.MBC.Interface (MBC (..), RAMAllocator, RTC (latchRTC, readRTC, writeRTC), RTCRegister (..))
import Machine.GBC.Util ((.<<.))

mbc3 :: Int -> Int -> RAMAllocator -> RTC -> IO MBC
mbc3 bankMask ramMask ramAllocator rtc = do
  romOffset <- newIORef 1
  register2 <- newIORef 0
  enableRAM <- newIORef False
  ram <- ramAllocator 0x8000

  let lowBankOffset = pure 0
  let highBankOffset = do
        offset <- readIORef romOffset
        pure ((offset .&. bankMask) .<<. 14)
  let getRAMOffset r2 = (r2 .&. ramMask) .<<. 13
  let ramBankOffset = getRAMOffset <$> readIORef register2
  let ramGate = readIORef enableRAM

  let writeROM address value
        | address < 0x2000 =
          writeIORef enableRAM (value .&. 0x0F == 0x0A)
        | address < 0x4000 =
          let offset = (fromIntegral value .&. 0x7F)
           in writeIORef romOffset (if offset == 0 then 1 else offset)
        | address < 0x6000 =
          writeIORef
            register2
            (fromIntegral (if value >= 8 && value <= 0xC then value else value .&. 3))
        | otherwise =
          latchRTC rtc value

  let readRAM address = do
        enabled <- readIORef enableRAM
        if not enabled
          then pure 0xFF
          else do
            r2 <- readIORef register2
            case decodeRTCRegister r2 of
              Nothing -> VSM.unsafeRead ram (getRAMOffset r2 + fromIntegral address)
              Just rtcRegister -> readRTC rtc rtcRegister
  let readRAMBankOffset offset address = VSM.unsafeRead ram (offset + fromIntegral address)
  let writeRAM address value = do
        enabled <- readIORef enableRAM
        when enabled $ do
          r2 <- readIORef register2
          case decodeRTCRegister r2 of
            Nothing -> VSM.unsafeWrite ram (getRAMOffset r2 + fromIntegral address) value
            Just rtcRegister -> writeRTC rtc rtcRegister value
  pure MBC {..}

decodeRTCRegister :: Int -> Maybe RTCRegister
decodeRTCRegister 0x08 = Just Seconds
decodeRTCRegister 0x09 = Just Minutes
decodeRTCRegister 0x0A = Just Hours
decodeRTCRegister 0x0B = Just DaysLow
decodeRTCRegister 0x0C = Just DaysHigh
decodeRTCRegister _ = Nothing
