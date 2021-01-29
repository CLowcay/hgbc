{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.DMA
  ( State (..),
    init,
    ports,
    update,
    doPending,
    doHBlank,
    makeHDMASource,
    makeHDMADestination,
  )
where

import Control.Monad.Reader (MonadIO (liftIO), ReaderT, when)
import Data.Bits (Bits ((.&.), (.|.)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word16, Word8)
import qualified Machine.GBC.Bus as Bus
import qualified Machine.GBC.Graphics.VRAM as VRAM
import qualified Machine.GBC.Memory as Memory
import Machine.GBC.Mode (EmulatorMode, cgbOnlyPort)
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import qualified Machine.GBC.Registers as R
import Machine.GBC.Util ((.<<.))
import Prelude hiding (init)

data PendingDMA = Pending !Word8 | None deriving (Eq, Ord, Show)

data State = State
  { hdmaActive :: !(IORef Bool),
    hdmaSource :: !(UnboxedRef Word16),
    hdmaDestination :: !(UnboxedRef Word16),
    dmaOffset :: !(UnboxedRef Word16),
    dmaBase :: !(UnboxedRef Word16),
    vram :: !VRAM.VRAM,
    pendingHDMA :: !(IORef PendingDMA),
    portDMA :: !Port,
    portHDMA1 :: !Port,
    portHDMA2 :: !Port,
    portHDMA3 :: !Port,
    portHDMA4 :: !Port,
    portHDMA5 :: !Port
  }

ports :: State -> [(Word16, Port)]
ports State {..} =
  [ (R.DMA, portDMA),
    (R.HDMA1, portHDMA1),
    (R.HDMA2, portHDMA2),
    (R.HDMA3, portHDMA3),
    (R.HDMA4, portHDMA4),
    (R.HDMA5, portHDMA5)
  ]

oamBytes :: Word16
oamBytes = 160

init :: VRAM.VRAM -> IORef EmulatorMode -> IO State
init vram modeRef = mdo
  hdmaActive <- newIORef False
  hdmaSource <- newUnboxedRef 0
  hdmaDestination <- newUnboxedRef 0
  dmaOffset <- newUnboxedRef 0
  dmaBase <- newUnboxedRef 0
  pendingHDMA <- newIORef None

  let loadHDMATargets = do
        hdma1 <- Port.read portHDMA1
        hdma2 <- Port.read portHDMA2
        hdma3 <- Port.read portHDMA3
        hdma4 <- Port.read portHDMA4
        writeUnboxedRef hdmaSource (makeHDMASource hdma1 hdma2)
        writeUnboxedRef hdmaDestination (makeHDMADestination hdma3 hdma4)

  portDMA <- Port.new 0x00 0xFF $ \_ dma -> do
    writeUnboxedRef dmaOffset (oamBytes + 2)
    writeUnboxedRef dmaBase (fromIntegral dma .<<. 8)
    pure dma

  portHDMA1 <- cgbOnlyPort modeRef 0x00 0xFF Port.alwaysUpdate
  portHDMA2 <- cgbOnlyPort modeRef 0x00 0xF0 Port.alwaysUpdate
  portHDMA3 <- cgbOnlyPort modeRef 0x00 0x1F Port.alwaysUpdate
  portHDMA4 <- cgbOnlyPort modeRef 0x00 0xF0 Port.alwaysUpdate
  portHDMA5 <- cgbOnlyPort modeRef 0x00 0xFF $ \_ hdma5' ->
    if hdma5' .&. 0x80 /= 0
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

  pure State {..}

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
doPending :: (Memory.Has env, Bus.Has env) => State -> ReaderT env IO ()
doPending State {..} = do
  maybeHDMA <- liftIO $ readIORef pendingHDMA
  case maybeHDMA of
    None -> pure ()
    Pending hdma -> do
      liftIO $ writeIORef pendingHDMA None
      source0 <- readUnboxedRef hdmaSource
      destination0 <- readUnboxedRef hdmaDestination
      go source0 destination0 (hdma + 1)
      Bus.delayClocks ((fromIntegral hdma + 1) * 8)
      where
        go _ _ 0 = pure ()
        go !source !destination !count = do
          Memory.copy16 source destination
          go (source + 16) (destination + 16) (count - 1)

-- | Notify the DMA controller that the LCD has entered the HBlank state. Return
-- the number of clock cycles to stall the CPU.
doHBlank :: (Memory.Has env, Bus.Has env) => State -> ReaderT env IO ()
doHBlank State {..} = do
  isActive <- liftIO $ readIORef hdmaActive
  when isActive $ do
    source <- readUnboxedRef hdmaSource
    destination <- readUnboxedRef hdmaDestination

    Memory.copy16 source destination

    liftIO $ do
      writeUnboxedRef hdmaSource (source + 16)
      writeUnboxedRef hdmaDestination (destination + 16)
      hdma5 <- Port.readDirect portHDMA5
      Port.writeDirect portHDMA5 (hdma5 - 1)
      when (hdma5 == 0) $ writeIORef hdmaActive False

    Bus.delayClocks 8

makeHDMASource :: Word8 -> Word8 -> Word16
makeHDMASource high low = (fromIntegral high .<<. 8) .|. (fromIntegral low .&. 0xF0)

makeHDMADestination :: Word8 -> Word8 -> Word16
makeHDMADestination high low =
  0x8000 + (((fromIntegral high .&. 0x1F) .<<. 8) .|. (fromIntegral low .&. 0xF0))
