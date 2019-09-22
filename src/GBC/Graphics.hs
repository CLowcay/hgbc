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
    lcdMode :: !Mode
  , clocksRemaining :: !Int
  , currentLine :: !Int
  , lineClocksRemaining :: !Int
  , vramDirty :: !Bool
} deriving (Eq, Ord, Show)

-- | The initial graphics state.
initGraphics :: GraphicsState
initGraphics = GraphicsState VBlank 153 0 0 False

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

totalLines, visibleLines, oamClocks, readClocks, hblankClocks, vblankClocks, lineClocks :: Int
totalLines = 154
visibleLines = 144
oamClocks = 80
readClocks = 172
hblankClocks = 204
vblankClocks = 69768
lineClocks = oamClocks + readClocks + hblankClocks

-- | Given a mode, the number of clocks remaining, and the number of elapsed
-- clocks, return the new mode and the new number of clocks remaining.
{-# INLINE nextMode #-}
nextMode :: Mode -> Int -> Int -> Int -> (Mode, Int)
nextMode mode remaining clocks line =
  let remaining' = remaining - clocks
  in  if remaining' > 0
        then (mode, remaining')
        else case mode of
          ScanOAM  -> (ReadVRAM, remaining' + readClocks)
          ReadVRAM -> (HBlank, remaining' + hblankClocks)
          HBlank   -> if line >= visibleLines
            then (VBlank, remaining' + vblankClocks)
            else (ScanOAM, remaining' + oamClocks)
          VBlank -> (ScanOAM, remaining' + oamClocks)

-- | Get the next LCD line given the current line, the clocks until the next
-- line, and the number of elapsed clocks.
{-# INLINE nextLine #-}
nextLine :: Int -> Int -> Int -> (Int, Int)
nextLine line remaining clocks =
  let remaining' = remaining - clocks
      line'      = line + 1
  in  if remaining' > 0
        then (line, remaining')
        else (if line' >= totalLines then 0 else line', remaining' + lineClocks)

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

{-# INLINABLE graphicsStep #-}
graphicsStep :: UsesGraphics env m => BusEvent -> ReaderT env m (Maybe Update)
graphicsStep (BusEvent newWrites clocks) = do
  graphicsState               <- asks forGraphicsState
  graphics@GraphicsState {..} <- liftIO $ readIORef graphicsState

  lcdEnabled                  <- testGraphicsFlag regLCDC flagLCDEnable
  let (mode', remaining')           = nextMode lcdMode clocksRemaining clocks currentLine
  let (line', lineClocksRemaining') = nextLine currentLine lineClocksRemaining clocks
  let vramDirty'                    = vramDirty || any isInVRAM newWrites

  if lcdEnabled
    then do
      when (currentLine /= line') $ writeMem regLY line'

      liftIO . writeIORef graphicsState $ GraphicsState
        { lcdMode             = mode'
        , clocksRemaining     = remaining'
        , currentLine         = line'
        , lineClocksRemaining = lineClocksRemaining'
        , vramDirty           = not $ mode' == ReadVRAM && vramDirty'
        }

      if lcdMode /= mode'
        then do
          setMode mode'
          pure . Just $ Update (mode' == ReadVRAM && vramDirty') mode'
        else pure Nothing
    else do
      liftIO . writeIORef graphicsState $ graphics { vramDirty = vramDirty' }
      pure Nothing
