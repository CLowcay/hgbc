{-# LANGUAGE TupleSections #-}
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
import           GBC.Primitive
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           Data.Functor
import           GBC.CPU
import           GBC.Memory
import           GBC.Registers

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data GraphicsState = GraphicsState {
    lcdState      :: !(StateCycle Mode)
  , lcdLine       :: !(StateCycle Word8)
  , lcdEnabledRef :: !(IORef Bool)
}

-- | Graphics synchronization objects. It's a pair of MVars. The CPU thread
-- takes from start and puts to currentLine. The graphics output thread takes
-- from currentLine and puts to start. This keeps the threads moving in
-- lockstep.
data GraphicsSync = GraphicsSync {
    hblankStart :: !(MVar ())        -- Graphics output puts a () here when it is safe to enter HBlank.
  , currentLine :: !(MVar Word8)     -- We put a line number here before we enter ScanOAM.
}

lcdStates :: [(Mode, Int)]
lcdStates =
  concat (replicate 144 [(ScanOAM, 80), (ReadVRAM, 172), (HBlank, 204)]) ++ [(VBlank, 4560)]

lcdLines :: [(Word8, Int)]
lcdLines = [0 .. 153] <&> (, 456)

-- | The initial graphics state.
initGraphics :: IO GraphicsState
initGraphics = do
  lcdState      <- newStateCycle lcdStates
  lcdLine       <- newStateCycle lcdLines
  lcdEnabledRef <- newIORef True
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

updateLY :: HasMemory env => Word8 -> ReaderT env IO ()
updateLY ly = do
  writeByte LY ly
  checkLY ly

checkLY :: HasMemory env => Word8 -> ReaderT env IO ()
checkLY ly = do
  lyc <- readByte LYC
  let matchFlag = if lyc == ly then bit matchBit else 0
  stat <- readByte STAT
  writeByte STAT (modifyBits (bit matchBit) matchFlag stat)
  when (stat `testBit` interruptCoincidence && lyc == ly) (raiseInterrupt 1)

{-# INLINABLE graphicsStep #-}
graphicsStep :: HasGraphics env => BusEvent -> ReaderT env IO ()
graphicsStep (BusEvent newWrites clocks _) = do
  GraphicsState {..} <- asks forGraphicsState
  graphicsSync       <- asks forGraphicsSync

  lcdEnabled         <- liftIO (readIORef lcdEnabledRef)
  for_ newWrites $ \case
    STAT -> do
      stat    <- readByte STAT
      lcdMode <- getStateCycle lcdState
      writeByte STAT (0x80 .|. setMode stat lcdMode)
    LCDC -> do
      lcdc <- readByte LCDC
      let lcdEnabled' = isFlagSet flagLCDEnable lcdc
      liftIO (writeIORef lcdEnabledRef lcdEnabled')
      when (lcdEnabled' && not lcdEnabled) $ do
        resetStateCycle lcdLine  lcdLines
        resetStateCycle lcdState lcdStates
        writeByte LY 0
      when (not lcdEnabled' && lcdEnabled) $ writeByte LY 0
    LYC -> readByte LY >>= checkLY
    _   -> pure ()

  when lcdEnabled $ do
    line' <- updateStateCycle lcdLine clocks updateLY
    void $ updateStateCycle lcdState clocks $ \mode' -> do
      -- Update STAT register
      stat <- readByte STAT
      writeByte STAT (modifyBits maskMode (modeBits mode') stat)

      -- If we're entering ReadVRAM mode, then signal the graphics output.
      when (mode' == ReadVRAM) $ liftIO $ putMVar (currentLine graphicsSync) line'

      -- Raise interrupts
      when (stat `testBit` interruptHBlank && mode' == HBlank) (raiseInterrupt 1)
      when (stat `testBit` interruptVBlank && mode' == VBlank) (raiseInterrupt 1)
      when (stat `testBit` interruptOAM && mode' == ScanOAM) (raiseInterrupt 1)
      when (mode' == VBlank) (raiseInterrupt 0)

      -- If we're entering HBlank mode, then sync
      when (mode' == HBlank) $ liftIO (takeMVar (hblankStart graphicsSync))

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
