{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , pattern LCDC
  , pattern STAT
  , pattern LY
  , pattern LYC
  , pattern SCX
  , pattern SCY
  , pattern WX
  , pattern WY
  , pattern BGP
  , pattern OBP0
  , pattern OBP1
  , flagLCDEnable
  , flagWindowTileMap
  , flagWindowEnable
  , flagTileDataSelect
  , flagBackgroundTileMap
  , flagOBJSize
  , flagOBJEnable
  , flagBackgroundEnable
  )
where

import           Common
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
} deriving Eq

-- | The initial graphics state.
initGraphics :: GraphicsState
initGraphics = GraphicsState VBlank 153 0 0

class HasGraphicsState env where
  forGraphicsState :: env -> IORef GraphicsState

type UsesGraphics env m = (UsesMemory env m, HasGraphicsState env)

-- | Notification that the graphics state has changed.
newtype Update = Update Mode deriving (Eq, Ord, Show)

oamClocks, readClocks, hblankClocks, vblankClocks, lineClocks :: Int
oamClocks = 80
readClocks = 172
hblankClocks = 204
vblankClocks = 4560
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

pattern LCDC :: Word16
pattern LCDC = 0xFF40

pattern STAT :: Word16
pattern STAT = 0xFF41

pattern LY :: Word16
pattern LY = 0xFF44

pattern LYC :: Word16
pattern LYC = 0xFF45

pattern SCX :: Word16
pattern SCX = 0xFF43

pattern SCY :: Word16
pattern SCY = 0xFF42

pattern WX :: Word16
pattern WX = 0xFF4B

pattern WY :: Word16
pattern WY = 0xFF4A

pattern BGP :: Word16
pattern BGP = 0xFF47

pattern OBP0 :: Word16
pattern OBP0 = 0xFF48

pattern OBP1 :: Word16
pattern OBP1 = 0xFF48

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
  GraphicsState {..} <- liftIO (readIORef graphicsState)

  lcdEnabled                  <- testGraphicsFlag LCDC flagLCDEnable
  let (mode', remaining')           = nextMode lcdMode clocksRemaining clocks lcdLine
  let (line', lineClocksRemaining') = nextLine lcdLine lineClocksRemaining clocks

  when (STAT `elem` newWrites) $ do
    stat <- readByte STAT
    writeMem STAT (setMode stat (if lcdEnabled then mode' else lcdMode))

  if not lcdEnabled
    then pure Nothing
    else do
      when (lcdLine /= line') $ writeMem LY line'

      liftIO . writeIORef graphicsState $ GraphicsState
        { lcdMode             = mode'
        , clocksRemaining     = remaining'
        , lcdLine             = line'
        , lineClocksRemaining = lineClocksRemaining'
        }

      if lcdMode == mode' then pure Nothing else do
          stat <- readByte STAT

          -- Update STAT register
          lyc  <- readByte LYC
          let matchFlag = if lyc == line' then bit matchBit else 0
          writeMem STAT
            $ modifyBits (bit matchBit .&. maskMode) (modeBits mode' .|. matchFlag) stat

          -- Raise interrupts
          when (stat `testBit` interruptCoincidence && lyc == line') $ raiseInterrupt 1
          when (stat `testBit` interruptHBlank && mode' == HBlank) $ raiseInterrupt 1
          when (stat `testBit` interruptVBlank && mode' == VBlank) $ raiseInterrupt 1
          when (stat `testBit` interruptOAM && mode' == ScanOAM) $ raiseInterrupt 1
          when (mode' == VBlank) $ raiseInterrupt 0

          pure . Just $ Update mode'
