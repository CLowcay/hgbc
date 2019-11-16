{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.GraphicsSync
  ( GraphicsState(..)
  , GraphicsSync(..)
  , HasGraphics(..)
  , Mode(..)
  , initGraphics
  , newGraphicsSync
  , graphicsRegisters
  , graphicsStep
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
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           GBC.CPU
import           GBC.Memory
import           GBC.Registers

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data GraphicsState = GraphicsState {
    lcdModeRef :: !(IORef Mode)
  , clocksRemainingRef :: !(IORef Int)
  , lcdLineRef :: !(IORef Word8)
  , lineClocksRemainingRef :: !(IORef Int)
  , lcdEnabledRef :: !(IORef Bool)
} deriving Eq

-- | Graphics synchronization objects. It's a pair of MVars. The CPU thread
-- takes from start and puts to currentLine. The graphics output thread takes
-- from currentLine and puts to start. This keeps the threads moving in
-- lockstep.
data GraphicsSync = GraphicsSync {
    hblankStart :: !(MVar ())        -- Graphics output puts a () here when it is safe to enter HBlank.
  , currentLine :: !(MVar Word8)     -- We put a line number here before we enter ScanOAM.
}

-- | The initial graphics state.
initGraphics :: IO GraphicsState
initGraphics = do
  lcdModeRef             <- newIORef ScanOAM
  clocksRemainingRef     <- newIORef oamClocks
  lcdLineRef             <- newIORef 0
  lineClocksRemainingRef <- newIORef lineClocks
  lcdEnabledRef          <- newIORef True
  pure GraphicsState { .. }

-- | Make a new Graphics sync object.
newGraphicsSync :: IO GraphicsSync
newGraphicsSync = do
  hblankStart <- newEmptyMVar
  currentLine <- newEmptyMVar
  pure GraphicsSync { .. }

class HasMemory env => HasGraphics env where
  forGraphicsState :: env -> GraphicsState
  forGraphicsSync :: env -> GraphicsSync

oamClocks, readClocks, hblankClocks, vblankClocks, lineClocks :: Int
oamClocks = 80
readClocks = 172
hblankClocks = 204
vblankClocks = 4560
lineClocks = oamClocks + readClocks + hblankClocks

totalLines, lastVisibleLine :: Word8
totalLines = 154
lastVisibleLine = 143

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
          HBlank   -> if line >= lastVisibleLine
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

-- | Prepare a status report on the graphics registers.
graphicsRegisters :: HasMemory env => ReaderT env IO [RegisterInfo]
graphicsRegisters = do
  lcdc <- readByte LCDC
  stat <- readByte STAT
  sequence
    [ pure (RegisterInfo LCDC "LCDC" lcdc (decodeLCDC lcdc))
    , pure (RegisterInfo STAT "STAT" stat (decodeSTAT stat))
    , RegisterInfo SCY "SCY" <$> readByte SCY <*> pure []
    , RegisterInfo SCX "SCX" <$> readByte SCX <*> pure []
    , RegisterInfo LY "LY" <$> readByte LY <*> pure []
    , RegisterInfo LYC "LYC" <$> readByte LYC <*> pure []
    , RegisterInfo DMA "DMA" <$> readByte DMA <*> pure []
    , RegisterInfo BGP "BGP" <$> readByte BGP <*> pure []
    , RegisterInfo OBP0 "OBP0" <$> readByte OBP0 <*> pure []
    , RegisterInfo OBP1 "OBP1" <$> readByte OBP1 <*> pure []
    , RegisterInfo WY "WY" <$> readByte WY <*> pure []
    , RegisterInfo WX "WX" <$> readByte WX <*> pure []
    , RegisterInfo VBK "VBK" <$> readByte VBK <*> pure []
    ]
 where
  decodeLCDC lcdc =
    [ ("LCD Enable"      , show $ 0 /= lcdc .&. flagLCDEnable)
    , ("Window Code Area", if 0 == lcdc .&. flagWindowTileMap then "9800" else "9C00")
    , ("Window Enable"   , show $ 0 /= lcdc .&. flagWindowEnable)
    , ( "Background Character Data Base"
      , if 0 == lcdc .&. flagTileDataSelect then "8800" else "8000"
      )
    , ("Background Code Area", if 0 == lcdc .&. flagBackgroundTileMap then "9800" else "9C00")
    , ("OBJ Height"          , if 0 == lcdc .&. flagOBJSize then "8" else "16")
    , ("OBJ Enable"          , show $ 0 /= lcdc .&. flagOBJEnable)
    , ("Background Enable"   , show $ 0 /= lcdc .&. flagBackgroundEnable)
    ]
  decodeSTAT stat =
    let mode = case stat .&. 0x03 of
          0 -> "HBlank"
          1 -> "VBlank"
          2 -> "Scanning OAM"
          3 -> "Reading VRAM"
          _ -> error "Impossible stat mode"
    in  [ ("Mode Flag"                , mode)
        , ("LYC = LY"                 , show $ stat `testBit` matchBit)
        , ("Interrupt on HBlank", show $ stat `testBit` interruptHBlank)
        , ("Interrupt on VBlank", show $ stat `testBit` interruptVBlank)
        , ("Interrupt on Scanning OAM", show $ stat `testBit` interruptOAM)
        , ("Interrupt on LYC = LY", show $ stat `testBit` interruptCoincidence)
        ]


{-# INLINABLE graphicsStep #-}
graphicsStep :: HasGraphics env => BusEvent -> ReaderT env IO ()
graphicsStep (BusEvent newWrites clocks _) = do
  GraphicsState {..}  <- asks forGraphicsState
  graphicsSync        <- asks forGraphicsSync

  lcdEnabled          <- liftIO (readIORef lcdEnabledRef)
  lcdMode             <- liftIO (readIORef lcdModeRef)
  clocksRemaining     <- liftIO (readIORef clocksRemainingRef)
  lcdLine             <- liftIO (readIORef lcdLineRef)
  lineClocksRemaining <- liftIO (readIORef lineClocksRemainingRef)

  let (mode', remaining')           = nextMode lcdMode clocksRemaining clocks lcdLine
  let (line', lineClocksRemaining') = nextLine lcdLine lineClocksRemaining clocks

  for_ newWrites $ \case
    STAT -> do
      stat <- readByte STAT
      writeByte STAT (setMode stat (if lcdEnabled then mode' else lcdMode))
    LCDC -> do
      lcdc <- readByte LCDC
      liftIO (writeIORef lcdEnabledRef (isFlagSet flagLCDEnable lcdc))
    _ -> pure ()

  when lcdEnabled $ do
    when (lcdLine /= line') $ writeByte LY line'

    liftIO $ do
      writeIORef lcdModeRef             mode'
      writeIORef clocksRemainingRef     remaining'
      writeIORef lcdLineRef             line'
      writeIORef lineClocksRemainingRef lineClocksRemaining'

    when (lcdMode /= mode') $ do
      stat <- readByte STAT

      -- Update STAT register
      lyc  <- readByte LYC
      let matchFlag = if lyc == line' then bit matchBit else 0
      writeByte STAT (modifyBits (bit matchBit .|. maskMode) (modeBits mode' .|. matchFlag) stat)

      -- If we're entering ReadVRAM mode, then signal the graphics output.
      when (mode' == ReadVRAM) $ liftIO $ putMVar (currentLine graphicsSync) line'

      -- Raise interrupts
      when (stat `testBit` interruptCoincidence && lyc == line') (raiseInterrupt 1)
      when (stat `testBit` interruptHBlank && mode' == HBlank) (raiseInterrupt 1)
      when (stat `testBit` interruptVBlank && mode' == VBlank) (raiseInterrupt 1)
      when (stat `testBit` interruptOAM && mode' == ScanOAM) (raiseInterrupt 1)
      when (mode' == VBlank) (raiseInterrupt 0)

      -- If we're entering HBlank mode, then sync
      when (mode' == HBlank) $ liftIO (takeMVar (hblankStart graphicsSync))
