{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module GBC.Graphics
  ( GraphicsState(..)
  , GraphicsSync(..)
  , Mode(..)
  , GraphicsBusEvent(..)
  , initGraphics
  , graphicsPorts
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
import           Data.Functor
import           Data.Word
import           GBC.Graphics.VRAM
import           GBC.Interrupts
import           GBC.Primitive
import           GBC.Registers

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data GraphicsState = GraphicsState {
    lcdState      :: !(StateCycle Mode)
  , lcdLine       :: !(StateCycle Word8)
  , portLCDC      :: !(Port Word8)
  , portSTAT      :: !(Port Word8)
  , portSCY       :: !(Port Word8)
  , portSCX       :: !(Port Word8)
  , portLY        :: !(Port Word8)
  , portLYC       :: !(Port Word8)
  , portBGP       :: !(Port Word8)
  , portOBP0      :: !(Port Word8)
  , portOBP1      :: !(Port Word8)
  , portWY        :: !(Port Word8)
  , portWX        :: !(Port Word8)
  , portBCPS      :: !(Port Word8)
  , portBCPD      :: !(Port Word8)
  , portOCPS      :: !(Port Word8)
  , portOCPD      :: !(Port Word8)
  , portVBK       :: !(Port Word8)
  , portIF        :: !(Port Word8)
  , vram          :: !VRAM
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
initGraphics :: VRAM -> Port Word8 -> IO GraphicsState
initGraphics vram portIF = mdo
  lcdState <- newStateCycle lcdStates
  lcdLine  <- newStateCycle lcdLines

  portLCDC <- newPort 0xFF 0xFF $ \lcdc lcdc' -> do
    let lcdEnabled  = isFlagSet flagLCDEnable lcdc
    let lcdEnabled' = isFlagSet flagLCDEnable lcdc'
    when (lcdEnabled' && not lcdEnabled) $ do
      resetStateCycle lcdLine  lcdLines
      resetStateCycle lcdState lcdStates
      directWritePort portLY 0
    when (not lcdEnabled' && lcdEnabled) $ directWritePort portLY 0
    pure lcdc'
  portSTAT <- newPort 0x00 0x78 alwaysUpdate
  portSCY  <- newPort 0x00 0xFF alwaysUpdate
  portSCX  <- newPort 0x00 0xFF alwaysUpdate
  portLY   <- newPort 0x00 0x00 neverUpdate
  portLYC  <- newPort 0x00 0xFF $ \_ lyc -> do
    ly <- directReadPort portLY
    checkLY portIF portSTAT ly lyc
    pure lyc
  portBGP  <- newPort 0xFF 0xFF alwaysUpdate
  portOBP0 <- newPort 0xFF 0xFF alwaysUpdate
  portOBP1 <- newPort 0xFF 0xFF alwaysUpdate
  portWY   <- newPort 0x00 0xFF alwaysUpdate
  portWX   <- newPort 0x00 0xFF alwaysUpdate
  portBCPS <- newPort 0x00 0xBF $ \_ bcps -> do
    directWritePort portBCPD =<< readPalette vram False bcps
    pure bcps
  portBCPD <- newPort 0x00 0xFF $ \_ bcpd -> do
    bcps <- readPort portBCPS
    writePalette vram False bcps bcpd
    when (isFlagSet flagPaletteIncrement bcps) $ writePort portBCPS ((bcps .&. 0xBF) + 1)
    pure bcpd
  portOCPS <- newPort 0x00 0xBF $ \_ ocps -> do
    directWritePort portOCPD =<< readPalette vram True ocps
    pure ocps
  portOCPD <- newPort 0x00 0xFF $ \_ ocpd -> do
    ocps <- readPort portOCPS
    writePalette vram True ocps ocpd
    when (isFlagSet flagPaletteIncrement ocps) $ writePort portOCPS ((ocps .&. 0xBF) + 1)
    pure ocpd
  portVBK <- newPort 0x00 0x01 $ \_ vbk -> do
    setVRAMBank vram (if vbk .&. 1 == 0 then 0 else 0x2000)
    pure vbk

  pure GraphicsState { .. }

graphicsPorts :: GraphicsState -> [(Word16, Port Word8)]
graphicsPorts GraphicsState {..} =
  [ (LCDC, portLCDC)
  , (STAT, portSTAT)
  , (SCY , portSCY)
  , (SCX , portSCX)
  , (LY  , portLY)
  , (LYC , portLYC)
  , (BGP , portBGP)
  , (OBP0, portOBP0)
  , (OBP1, portOBP1)
  , (WY  , portWY)
  , (WX  , portWX)
  , (BCPS, portBCPS)
  , (BCPD, portBCPD)
  , (OCPS, portOCPS)
  , (OCPD, portOCPD)
  , (VBK , portVBK)
  ]

-- | Make a new Graphics sync object.
newGraphicsSync :: IO GraphicsSync
newGraphicsSync = do
  hblankStart <- newEmptyMVar
  currentLine <- newEmptyMVar
  pure GraphicsSync { .. }

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

flagPaletteIncrement :: Word8
flagPaletteIncrement = 0x80

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

-- | Get the bit code corresponding to the LCD mode.
modeBits :: Mode -> Word8
modeBits HBlank   = 0
modeBits VBlank   = 1
modeBits ScanOAM  = 2
modeBits ReadVRAM = 3

updateLY :: Port Word8 -> Port Word8 -> Port Word8 -> Port Word8 -> Word8 -> IO ()
updateLY portIF portLY portLYC portSTAT ly = do
  directWritePort portLY ly
  lyc <- readPort portLYC
  checkLY portIF portSTAT ly lyc

checkLY :: Port Word8 -> Port Word8 -> Word8 -> Word8 -> IO ()
checkLY portIF portSTAT ly lyc = do
  let matchFlag = if lyc == ly then bit matchBit else 0
  stat <- readPort portSTAT
  directWritePort portSTAT (modifyBits (bit matchBit) matchFlag stat)
  when (stat `testBit` interruptCoincidence && lyc == ly) (raiseInterrupt portIF InterruptLCDCStat)

data GraphicsBusEvent = NoGraphicsEvent | HBlankEvent deriving (Eq, Ord, Show)

{-# INLINABLE graphicsStep #-}
graphicsStep :: GraphicsState -> GraphicsSync -> Int -> IO GraphicsBusEvent
graphicsStep GraphicsState {..} graphicsSync clockAdvance = do
  lcdc <- readPort portLCDC
  let lcdEnabled = isFlagSet flagLCDEnable lcdc
  if not lcdEnabled
    then pure NoGraphicsEvent
    else do
      line' <- getUpdateResult
        <$> updateStateCycle lcdLine clockAdvance (updateLY portIF portLY portLYC portSTAT)
      modeUpdate <- updateStateCycle lcdState clockAdvance $ \mode' -> do
        -- Update STAT register
        stat <- readPort portSTAT
        directWritePort portSTAT (modifyBits maskMode (modeBits mode') stat)

        -- If we're entering ReadVRAM mode, then signal the graphics output.
        when (mode' == ReadVRAM) $ do
          setVRAMAccessible vram False
          putMVar (currentLine graphicsSync) line'

        -- Raise interrupts
        when (stat `testBit` interruptHBlank && mode' == HBlank)
             (raiseInterrupt portIF InterruptLCDCStat)
        when (stat `testBit` interruptVBlank && mode' == VBlank)
             (raiseInterrupt portIF InterruptLCDCStat)
        when (stat `testBit` interruptOAM && mode' == ScanOAM)
             (raiseInterrupt portIF InterruptLCDCStat)
        when (mode' == VBlank) (raiseInterrupt portIF InterruptVBlank)

        -- If we're entering HBlank mode, then sync
        when (mode' == HBlank) $ do
          takeMVar (hblankStart graphicsSync)
          setVRAMAccessible vram True

      pure $ case modeUpdate of
        HasChangedTo HBlank -> HBlankEvent
        _                   -> NoGraphicsEvent

-- | Prepare a status report on the graphics registers.
graphicsRegisters :: GraphicsState -> IO [RegisterInfo]
graphicsRegisters GraphicsState {..} = do
  lcdc <- readPort portLCDC
  stat <- readPort portSTAT
  sequence
    [ pure (RegisterInfo LCDC "LCDC" lcdc (decodeLCDC lcdc))
    , pure (RegisterInfo STAT "STAT" stat (decodeSTAT stat))
    , RegisterInfo SCY "SCY" <$> readPort portSCY <*> pure []
    , RegisterInfo SCX "SCX" <$> readPort portSCX <*> pure []
    , RegisterInfo LY "LY" <$> readPort portLY <*> pure []
    , RegisterInfo LYC "LYC" <$> readPort portLYC <*> pure []
    , RegisterInfo BGP "BGP" <$> readPort portBGP <*> pure []
    , RegisterInfo OBP0 "OBP0" <$> readPort portOBP0 <*> pure []
    , RegisterInfo OBP1 "OBP1" <$> readPort portOBP1 <*> pure []
    , RegisterInfo WY "WY" <$> readPort portWY <*> pure []
    , RegisterInfo WX "WX" <$> readPort portWX <*> pure []
    , RegisterInfo VBK "VBK" <$> readPort portVBK <*> pure []
    ]
 where
  decodeLCDC lcdc =
    [ ("LCD Enable"      , show $ isFlagSet flagLCDEnable lcdc)
    , ("Window Code Area", if 0 == lcdc .&. flagWindowTileMap then "9800" else "9C00")
    , ("Window Enable"   , show $ isFlagSet flagWindowEnable lcdc)
    , ( "Background Character Data Base"
      , if 0 == lcdc .&. flagTileDataSelect then "8800" else "8000"
      )
    , ("Background Code Area", if 0 == lcdc .&. flagBackgroundTileMap then "9800" else "9C00")
    , ("OBJ Height"          , if 0 == lcdc .&. flagOBJSize then "8" else "16")
    , ("OBJ Enable"          , show $ isFlagSet flagOBJEnable lcdc)
    , ("Background Enable"   , show $ isFlagSet flagBackgroundEnable lcdc)
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
