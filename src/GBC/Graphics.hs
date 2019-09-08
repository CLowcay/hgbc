{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Graphics
  ( Graphics
  , GraphicsState(..)
  , Mode(..)
  , Update(..)
  , Synchronizer(..)
  , initGraphics
  , graphicsStep
  )
where

import qualified Data.IntSet                   as S
import           Control.Concurrent.MVar
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           GBC.Memory
import           GBC.CPU
import           Data.Word
import           Data.Bits
import           Data.Maybe

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data GraphicsState = GraphicsState {
    lcdTime :: !Int
  , writes :: ![Word16]
} deriving (Eq, Ord, Show)

-- | The initial graphics state.
initGraphics :: GraphicsState
initGraphics = GraphicsState 0 []

-- | Notification that the graphics state has changed.
data Update = Update {
    updateCharacters :: !S.IntSet
  , updateMode :: !Mode
} deriving (Eq, Ord, Show)

-- | Construct an 'Update'.
makeUpdate :: Mode -> [Word16] -> Update
makeUpdate mode writeAddrs = Update chars mode
 where
  chars = S.fromList . catMaybes $ getCharNumber <$> writeAddrs
  getCharNumber addr = if addr >= 0x8000 && addr < 0x9800
    then Just . fromIntegral $ (addr - 0x8000) `div` 16
    else Nothing

-- | An object to synchronize the actual display windows with the rest of the
-- simulation.
data Synchronizer = Synchronizer {
    updateInfo :: !(MVar Update)   -- ^ Write an 'Update' here to send the update to the window.
  , updateAck :: !(MVar ())        -- ^ Wait for an acknowledgement to show up here.
}

-- | The Graphics monad.
type Graphics a = ReaderT Memory (StateT GraphicsState IO) a

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

testGraphicsFlag :: Word16 -> Word8 -> Graphics Bool
testGraphicsFlag reg flag = isFlagSet flag <$> readByte reg

setBits :: Word8 -> Word8 -> Word8 -> Word8
setBits mask value source = value .|. (source .&. complement mask)

setMode :: (MonadReader Memory m, MonadIO m) => Mode -> m ()
setMode HBlank   = writeMem regSTAT =<< (setBits maskMode 0 <$> readByte regSTAT)
setMode VBlank   = writeMem regSTAT =<< (setBits maskMode 1 <$> readByte regSTAT)
setMode ScanOAM  = writeMem regSTAT =<< (setBits maskMode 2 <$> readByte regSTAT)
setMode ReadVRAM = writeMem regSTAT =<< (setBits maskMode 3 <$> readByte regSTAT)

graphicsStep :: BusEvent -> Graphics (Maybe Update)
graphicsStep (BusEvent _ newWrites clocks) = do
  GraphicsState {..} <- get
  lcdEnabled         <- testGraphicsFlag regLCDC flagLCDEnable
  if not lcdEnabled
    then pure Nothing
    else do
      let lcdTime'       = (lcdTime + clocks) `mod` totalClocks
      let (mode, line)   = modeAt lcdTime
      let (mode', line') = modeAt lcdTime'
      let writes'        = newWrites ++ writes

      when (line /= line') $ writeMem regLY line'

      if mode /= mode'
        then do
          put $ GraphicsState lcdTime' []
          setMode mode'
          pure . Just $ makeUpdate mode' writes'
        else do
          put $ GraphicsState lcdTime' writes'
          pure Nothing
