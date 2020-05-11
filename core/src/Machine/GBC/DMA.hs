{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.DMA
  ( State(..)
  , init
  , ports
  , update
  , doPendingHDMA
  , doHBlankHDMA
  , makeHDMASource
  , makeHDMADestination
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Registers
import           Machine.GBC.Util
import           Prelude                 hiding ( init )
import qualified Machine.GBC.Bus               as Bus
import qualified Machine.GBC.Graphics.VRAM     as VRAM
import qualified Machine.GBC.Memory            as Memory

data PendingDMA = Pending !Word8 | None deriving (Eq, Ord, Show)

data State = State {
    hdmaActive      :: !(IORef Bool)
  , hdmaSource      :: !(UnboxedRef Word16)
  , hdmaDestination :: !(UnboxedRef Word16)
  , dmaOffset       :: !(UnboxedRef Word16)
  , dmaBase         :: !(UnboxedRef Word16)
  , vram            :: !VRAM.VRAM
  , pendingHDMA     :: !(IORef PendingDMA)
  , portDMA         :: !(Port Word8)
  , portHDMA1       :: !(Port Word8)
  , portHDMA2       :: !(Port Word8)
  , portHDMA3       :: !(Port Word8)
  , portHDMA4       :: !(Port Word8)
  , portHDMA5       :: !(Port Word8)
}

ports :: State -> [(Word16, Port Word8)]
ports State {..} =
  [ (DMA  , portDMA)
  , (HDMA1, portHDMA1)
  , (HDMA2, portHDMA2)
  , (HDMA3, portHDMA3)
  , (HDMA4, portHDMA4)
  , (HDMA5, portHDMA5)
  ]

oamBytes :: Word16
oamBytes = 160

init :: VRAM.VRAM -> IO State
init vram = mdo
  hdmaActive      <- newIORef False
  hdmaSource      <- newUnboxedRef 0
  hdmaDestination <- newUnboxedRef 0
  dmaOffset       <- newUnboxedRef 0
  dmaBase         <- newUnboxedRef 0
  pendingHDMA     <- newIORef None

  let loadHDMATargets = do
        hdma1 <- readPort portHDMA1
        hdma2 <- readPort portHDMA2
        hdma3 <- readPort portHDMA3
        hdma4 <- readPort portHDMA4
        writeUnboxedRef hdmaSource      (makeHDMASource hdma1 hdma2)
        writeUnboxedRef hdmaDestination (makeHDMADestination hdma3 hdma4)

  portDMA <- newPort 0x00 0xFF $ \_ dma -> do
    writeUnboxedRef dmaOffset (oamBytes + 2)
    writeUnboxedRef dmaBase   (fromIntegral dma .<<. 8)
    pure dma

  portHDMA1 <- newPort 0x00 0xFF alwaysUpdate
  portHDMA2 <- newPort 0x00 0xF0 alwaysUpdate
  portHDMA3 <- newPort 0x00 0x1F alwaysUpdate
  portHDMA4 <- newPort 0x00 0xF0 alwaysUpdate
  portHDMA5 <- newPort 0x00 0xFF $ \_ hdma5' -> if hdma5' .&. 0x80 /= 0
    then do
      loadHDMATargets
      writeIORef hdmaActive True
      pure (hdma5' .&. 0x7F)
    else do
      isActive <- readIORef hdmaActive
      if isActive
        then do
          writeIORef hdmaActive False
          pure (hdma5' .|. 0x80)
        else do
          loadHDMATargets
          writeIORef pendingHDMA $! Pending hdma5'
          pure 0xFF

  pure State { .. }

update :: Memory.Has env => State -> ReaderT env IO ()
update State {..} = do
  offset <- readUnboxedRef dmaOffset
  when (offset > 0) $ do
    base <- readUnboxedRef dmaBase
    when (offset <= oamBytes) $ do
      let baseOffset = oamBytes - offset
      liftIO . VRAM.writeOAMDirect vram baseOffset =<< Memory.readByte (base + baseOffset)

    let offset' = offset - 1
    liftIO $ do
      when (offset' == oamBytes) $ VRAM.setOAMAccessible vram False
      when (offset' == 0) $ VRAM.setOAMAccessible vram True
    writeUnboxedRef dmaOffset offset'

-- | Perform any pending HDMA actions for this emulation cycle, and return the
-- number of clocks to stall the CPU.
doPendingHDMA :: (Memory.Has env, Bus.Has env) => State -> ReaderT env IO ()
doPendingHDMA State {..} = do
  maybeHDMA <- liftIO $ readIORef pendingHDMA
  case maybeHDMA of
    None         -> pure ()
    Pending hdma -> do
      liftIO $ writeIORef pendingHDMA None
      source0      <- readUnboxedRef hdmaSource
      destination0 <- readUnboxedRef hdmaDestination
      go source0 destination0 (hdma + 1)
      Bus.delayClocks ((fromIntegral hdma + 1) * 8)
     where
      go _       _            0      = pure ()
      go !source !destination !count = do
        Memory.copy16 source destination
        go (source + 16) (destination + 16) (count - 1)

-- | Notify the DMA controller that the LCD has entered the HBlank state. Return
-- the number of clock cycles to stall the CPU.
doHBlankHDMA :: (Memory.Has env, Bus.Has env) => State -> ReaderT env IO ()
doHBlankHDMA State {..} = do
  isActive <- liftIO $ readIORef hdmaActive
  when isActive $ do
    source      <- readUnboxedRef hdmaSource
    destination <- readUnboxedRef hdmaDestination

    Memory.copy16 source destination

    liftIO $ do
      writeUnboxedRef hdmaSource      (source + 16)
      writeUnboxedRef hdmaDestination (destination + 16)
      hdma5 <- directReadPort portHDMA5
      directWritePort portHDMA5 (hdma5 - 1)
      when (hdma5 == 0) $ writeIORef hdmaActive False

    Bus.delayClocks 8

makeHDMASource :: Word8 -> Word8 -> Word16
makeHDMASource high low = (fromIntegral high .<<. 8) .|. (fromIntegral low .&. 0xF0)

makeHDMADestination :: Word8 -> Word8 -> Word16
makeHDMADestination high low =
  0x8000 + (((fromIntegral high .&. 0x1F) .<<. 8) .|. (fromIntegral low .&. 0xF0))
