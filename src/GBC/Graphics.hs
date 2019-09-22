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
  , lcdLine :: !Word8
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

oamClocks, readClocks, hblankClocks, vblankClocks, lineClocks :: Int
oamClocks = 80
readClocks = 172
hblankClocks = 204
vblankClocks = 69768
lineClocks = oamClocks + readClocks + hblankClocks

totalLines, visibleLines :: Word8
totalLines = 154
visibleLines = 144

-- | Given a mode, the number of clocks remaining, and the number of elapsed
-- clocks, return the new mode and the new number of clocks remaining.
{-# INLINE nextMode #-}
nextMode :: Mode -> Int -> Int -> Word8 -> (Mode, Int)
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
nextLine :: Word8 -> Int -> Int -> (Word8, Int)
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

regLYC :: Word16
regLYC = 0xFF45

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

matchBit, interruptCoincidence, interruptOAM, interruptVBlank, interruptHBlank :: Int
matchBit = 2
interruptCoincidence = 6
interruptOAM = 5
interruptVBlank = 4
interruptHBlank = 3

maskMode :: Word8
maskMode = 0x03

isFlagSet :: Word8 -> Word8 -> Bool
isFlagSet flag v = v .&. flag /= 0

{-# INLINABLE testGraphicsFlag #-}
testGraphicsFlag :: UsesMemory env m => Word16 -> Word8 -> ReaderT env m Bool
testGraphicsFlag reg flag = isFlagSet flag <$> readByte reg

-- | Modify some bits with a mask.
modifyBits :: Word8 -> Word8 -> Word8 -> Word8
modifyBits mask value source = value .|. (source .&. complement mask)

-- | Set the LCD mode bits.
setMode :: Word8 -> Mode -> Word8
setMode v mode = modifyBits maskMode (modeBits mode) v

-- | Get the bit code corresponding to the LCD mode.
modeBits :: Mode -> Word8
modeBits HBlank   = 0
modeBits VBlank   = 1
modeBits ScanOAM  = 2
modeBits ReadVRAM = 3

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
  let (mode', remaining')           = nextMode lcdMode clocksRemaining clocks lcdLine
  let (line', lineClocksRemaining') = nextLine lcdLine lineClocksRemaining clocks
  let vramDirty'                    = vramDirty || any isInVRAM newWrites

  when (regSTAT `elem` newWrites) $ do
    stat <- readByte regSTAT
    writeMem regSTAT $ setMode stat $ if lcdEnabled then mode' else lcdMode

  if lcdEnabled
    then do
      when (lcdLine /= line') $ writeMem regLY line'

      liftIO . writeIORef graphicsState $ GraphicsState
        { lcdMode             = mode'
        , clocksRemaining     = remaining'
        , lcdLine             = line'
        , lineClocksRemaining = lineClocksRemaining'
        , vramDirty           = not $ mode' == ReadVRAM && vramDirty'
        }

      if lcdMode /= mode'
        then do
          stat <- readByte regSTAT

          -- Update STAT register
          lyc <- readByte regLYC
          let matchFlag = if lyc == line' then bit matchBit else 0
          writeMem regSTAT
            $ modifyBits (bit matchBit .&. maskMode) (modeBits mode' .|. matchFlag) stat

          -- Raise interrupts
          when (stat `testBit` interruptCoincidence && lyc == line') $ raiseInterrupt 1
          when (stat `testBit` interruptHBlank && mode' == HBlank) $ raiseInterrupt 1
          when (stat `testBit` interruptVBlank && mode' == VBlank) $ raiseInterrupt 1
          when (stat `testBit` interruptOAM && mode' == ScanOAM) $ raiseInterrupt 1
          when (mode' == VBlank) $ raiseInterrupt 0

          pure . Just $ Update (mode' == ReadVRAM && vramDirty') mode'
        else pure Nothing
    else do
      liftIO . writeIORef graphicsState $ graphics { vramDirty = vramDirty' }
      pure Nothing
