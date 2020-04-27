{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.DMA
  ( DMAState(..)
  , initDMA
  , dmaPorts
  , doPendingDMA
  , doHBlankHDMA
  , makeHDMASource
  , makeHDMADestination
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.Memory
import           Machine.GBC.Primitive
import           Machine.GBC.Registers
import           Machine.GBC.Util

data PendingDMA = Pending !Word8 | None deriving (Eq, Ord, Show)

data DMAState = DMAState {
    hdmaActive      :: !(IORef Bool)
  , hdmaSource      :: !(IORef Word16)
  , hdmaDestination :: !(IORef Word16)
  , pendingDMA      :: !(IORef PendingDMA)
  , pendingHDMA     :: !(IORef PendingDMA)
  , portDMA         :: !(Port Word8)
  , portHDMA1       :: !(Port Word8)
  , portHDMA2       :: !(Port Word8)
  , portHDMA3       :: !(Port Word8)
  , portHDMA4       :: !(Port Word8)
  , portHDMA5       :: !(Port Word8)
}

dmaPorts :: DMAState -> [(Word16, Port Word8)]
dmaPorts DMAState {..} =
  [ (DMA  , portDMA)
  , (HDMA1, portHDMA1)
  , (HDMA2, portHDMA2)
  , (HDMA3, portHDMA3)
  , (HDMA4, portHDMA4)
  , (HDMA5, portHDMA5)
  ]

initDMA :: IO DMAState
initDMA = mdo
  hdmaActive      <- newIORef False
  hdmaSource      <- newIORef 0
  hdmaDestination <- newIORef 0
  pendingDMA      <- newIORef None
  pendingHDMA     <- newIORef None

  let loadHDMATargets = do
        hdma1 <- readPort portHDMA1
        hdma2 <- readPort portHDMA2
        hdma3 <- readPort portHDMA3
        hdma4 <- readPort portHDMA4
        writeIORef hdmaSource $! makeHDMASource hdma1 hdma2
        writeIORef hdmaDestination $! makeHDMADestination hdma3 hdma4

  portDMA <- newPort 0x00 0xFF $ \_ dma -> do
    writeIORef pendingDMA $! Pending dma
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

  pure DMAState { .. }

-- | Perform any pending DMA actions for this emulation cycle, and return the
-- number of clocks to stall the CPU.
doPendingDMA :: HasMemory env => DMAState -> ReaderT env IO Int
doPendingDMA DMAState {..} = do
  maybeDMA <- liftIO $ readIORef pendingDMA
  case maybeDMA of
    None        -> pure ()
    Pending dma -> do
      liftIO $ writeIORef pendingDMA None
      dmaToOAM (fromIntegral dma `shiftL` 8)

  maybeHDMA <- liftIO $ readIORef pendingHDMA
  case maybeHDMA of
    None         -> pure 0
    Pending hdma -> do
      liftIO $ writeIORef pendingHDMA None
      source0      <- liftIO $ readIORef hdmaSource
      destination0 <- liftIO $ readIORef hdmaDestination
      go source0 destination0 (hdma + 1)
     where
      go _       _            0      = pure ((fromIntegral hdma + 1) * 8)
      go !source !destination !count = do
        copy16 source destination
        go (source + 16) (destination + 16) (count - 1)

-- | Notify the DMA controller that the LCD has entered the HBlank state. Return
-- the number of clock cycles to stall the CPU.
doHBlankHDMA :: HasMemory env => DMAState -> ReaderT env IO Int
doHBlankHDMA DMAState {..} = do
  isActive <- liftIO $ readIORef hdmaActive
  if not isActive
    then pure 0
    else do
      source      <- liftIO $ readIORef hdmaSource
      destination <- liftIO $ readIORef hdmaDestination

      copy16 source destination

      liftIO $ do
        writeIORef hdmaSource $! source + 16
        writeIORef hdmaDestination $! destination + 16
        hdma5 <- directReadPort portHDMA5
        directWritePort portHDMA5 (hdma5 - 1)
        when (hdma5 == 0) $ writeIORef hdmaActive False

      pure 8

makeHDMASource :: Word8 -> Word8 -> Word16
makeHDMASource high low = (fromIntegral high .<<. 8) .|. (fromIntegral low .&. 0xF0)

makeHDMADestination :: Word8 -> Word8 -> Word16
makeHDMADestination high low =
  0x8000 + (((fromIntegral high .&. 0x1F) .<<. 8) .|. (fromIntegral low .&. 0xF0))
