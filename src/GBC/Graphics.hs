{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

module GBC.Graphics
  ( GraphicsState(..)
  , HasGraphicsState(..)
  , UsesGraphics
  , Mode(..)
  , Update(..)
  , initGraphics
  , graphicsStep
  , decodeVRAM
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Traversable
import           Data.Word
import           GBC.CPU
import           GBC.Memory

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data GraphicsState = GraphicsState {
    lcdTime :: !Int
  , vramDirty :: !Bool
} deriving (Eq, Ord, Show)

-- | The initial graphics state.
initGraphics :: GraphicsState
initGraphics = GraphicsState 0 False

class HasGraphicsState env where
  forGraphicsState :: env -> IORef GraphicsState

type UsesGraphics env m = (UsesMemory env m, HasGraphicsState env)

-- | Notification that the graphics state has changed.
data Update = Update {
    updateVRAM :: !Bool
  , updateMode :: !Mode
} deriving (Eq, Ord, Show)

isInVRAM :: Word16 -> Bool
isInVRAM addr = addr >= 0x8000 && addr < 0x9800

totalLines, oamClocks, readClocks, hblankClocks, totalClocks :: Int
totalLines = 144
oamClocks = 80
readClocks = 172 + oamClocks
hblankClocks = 204 + readClocks
totalClocks = 70224

-- | Get the mode after a given number of clock cycles.
modeAt :: Int -> (Mode, Word8)
modeAt x =
  let (line, inLine) = x `divMod` hblankClocks
  in  (, fromIntegral line) $ if x < totalLines * hblankClocks
        then if
          | inLine < oamClocks  -> ScanOAM
          | inLine < readClocks -> ReadVRAM
          | otherwise           -> HBlank
        else VBlank

regLCDC :: Word16
regLCDC = 0xFF40

regSTAT :: Word16
regSTAT = 0xFF41

regLY :: Word16
regLY = 0xFF44

flagLCDEnable, flagWindowTileMap, flagWindowEnable, flagTileDataSelect, flagBackgroundTileMap, flagOBJSize, flagOBJEnable, flagBackgroundEnable
  :: Word8
flagLCDEnable = 0x80
flagWindowTileMap = 0x40
flagWindowEnable = 0x20
flagTileDataSelect = 0x10
flagBackgroundTileMap = 0x08
flagOBJSize = 0x04
flagOBJEnable = 0x02
flagBackgroundEnable = 0x01

maskMode :: Word8
maskMode = 0x03

isFlagSet :: Word8 -> Word8 -> Bool
isFlagSet flag v = v .&. flag /= 0

{-# INLINABLE testGraphicsFlag #-}
testGraphicsFlag :: UsesMemory env m => Word16 -> Word8 -> ReaderT env m Bool
testGraphicsFlag reg flag = isFlagSet flag <$> readByte reg

setBits :: Word8 -> Word8 -> Word8 -> Word8
setBits mask value source = value .|. (source .&. complement mask)

{-# INLINABLE setMode #-}
setMode :: UsesMemory env m => Mode -> ReaderT env m ()
setMode HBlank   = writeMem regSTAT =<< (setBits maskMode 0 <$> readByte regSTAT)
setMode VBlank   = writeMem regSTAT =<< (setBits maskMode 1 <$> readByte regSTAT)
setMode ScanOAM  = writeMem regSTAT =<< (setBits maskMode 2 <$> readByte regSTAT)
setMode ReadVRAM = writeMem regSTAT =<< (setBits maskMode 3 <$> readByte regSTAT)

{-# INLINABLE decodeVRAM #-}
decodeVRAM :: UsesMemory env m => ReaderT env m [[Word8]]
decodeVRAM = for [ (x, y, yi) | yi <- [0 .. 23], y <- [0 .. 7], x <- [0 .. 15] ] $ \(x, y, yi) ->
  do
    (l, h) <- readWord $ x * 16 + y * 2 + yi * 256
    pure $ zipWith combine (decodeByte l) (decodeByte h)
 where
  decodeByte :: Word8 -> [Word8]
  decodeByte byte = (\t -> if byte `testBit` t then 1 else 0) <$> [7, 6 .. 0]
  combine byteL byteH = 64 * (byteL .|. (byteH `shiftL` 1))
  readWord offset = (,) <$> readByte (0x8000 + offset) <*> readByte (0x8001 + offset)

{-# INLINEABLE graphicsStep #-}
graphicsStep :: UsesGraphics env m => BusEvent -> ReaderT env m (Maybe Update)
graphicsStep (BusEvent _ newWrites clocks) = do
  graphicsState      <- asks forGraphicsState
  GraphicsState {..} <- liftIO $ readIORef graphicsState

  lcdEnabled         <- testGraphicsFlag regLCDC flagLCDEnable
  let lcdTime'       = (lcdTime + clocks) `mod` totalClocks
  let (mode, line)   = modeAt lcdTime
  let (mode', line') = modeAt lcdTime'
  let vramDirty'     = vramDirty || any isInVRAM newWrites

  if lcdEnabled
    then do
      when (line /= line') $ writeMem regLY line'
      if mode /= mode'
        then do
          liftIO . writeIORef graphicsState $ GraphicsState
            lcdTime'
            (not $ mode' == ReadVRAM && vramDirty')
          setMode mode'
          pure . Just $ Update (mode' == ReadVRAM && vramDirty') mode'
        else do
          liftIO . writeIORef graphicsState $ GraphicsState lcdTime' vramDirty'
          pure Nothing
    else do
      liftIO . writeIORef graphicsState $ GraphicsState lcdTime vramDirty'
      pure Nothing
