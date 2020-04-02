{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Machine.GBC.Graphics
  ( GraphicsState(..)
  , GraphicsSync(..)
  , Mode(..)
  , GraphicsBusEvent(..)
  , initGraphics
  , graphicsPorts
  , newGraphicsSync
  , graphicsRegisters
  , graphicsStep
  )
where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Bits
import           Data.Functor
import           Data.IORef
import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Machine.GBC.CPU.Interrupts
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Mode
import           Machine.GBC.Primitive
import           Machine.GBC.Registers
import           Machine.GBC.Util
import qualified Data.Vector.Unboxed.Mutable   as VUM

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
  , modeRef       :: !(IORef EmulatorMode)
  , frameBufferBytes :: !(Ptr Word8)

  -- | A temporary area for blending the background, window, and sprites.
  , assemblySpace :: !(VUM.IOVector PixelInfo)
  -- | Each byte in this buffer corresponds to one of the pixels in the
  -- lineAssembly space. When a sprite pixel is drawn, the x-position of that
  -- sprite is written to the spritePriorityBuffer. This allows us to simulate
  -- the DMG behavior where sprites with lower x-positions take priority.
  , priorityBuffer :: !(VUM.IOVector Int8)
}

type Index = Word8     -- Values ranging from 0 ~ 3
type Palette = Word8   -- Values ranging from 0 ~ 7, only valid for CGB.
type Layer = Word8     -- 0, 1, or 2.  0 for background, 1 for sprite layer 1, 2 for sprite layer 2.
type BGPriority = Bool -- True if the background layer has priority.
type PixelInfo = (Index, (Layer, Palette, BGPriority))

-- | Graphics synchronization objects. The output thread should wait on
-- signalWindow, then draw the buffer, then put to bufferAvailable when it is
-- safe to write to the frame buffer again.
data GraphicsSync = GraphicsSync {
    bufferAvailable :: !(MVar ())    -- ^ The output window puts a () here when it is safe to write to the frame buffer again.
  , signalWindow    :: !(MVar ())    -- ^ Wait on this MVar before drawing the output buffer.
}

lcdStates :: [(Mode, Int)]
lcdStates =
  concat (replicate 144 [(ScanOAM, 80), (ReadVRAM, 172), (HBlank, 204)]) ++ [(VBlank, 4560)]

lcdLines :: [(Word8, Int)]
lcdLines = [0 .. 153] <&> (, 456)

-- | The initial graphics state.
initGraphics :: VRAM -> IORef EmulatorMode -> Ptr Word8 -> Port Word8 -> IO GraphicsState
initGraphics vram modeRef frameBufferBytes portIF = mdo
  lcdState <- newStateCycle lcdStates
  lcdLine  <- newStateCycle lcdLines

  -- A port that is only available in CGB mode.

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
  portBCPS <- cgbOnlyPort modeRef 0x00 0xBF
    $ \_ bcps' -> bcps' <$ (directWritePort portBCPD =<< readPalette vram False bcps')
  portBCPD <- cgbOnlyPort modeRef 0x00 0xFF $ \_ bcpd' -> bcpd' <$ do
    bcps <- readPort portBCPS
    writePalette vram False bcps bcpd'
    when (isFlagSet flagPaletteIncrement bcps) $ writePort portBCPS ((bcps .&. 0xBF) + 1)
  portOCPS <- cgbOnlyPort modeRef 0x00 0xBF
    $ \_ ocps' -> ocps' <$ (directWritePort portOCPD =<< readPalette vram True ocps')
  portOCPD <- cgbOnlyPort modeRef 0x00 0xFF $ \_ ocpd' -> ocpd' <$ do
    ocps <- readPort portOCPS
    writePalette vram True ocps ocpd'
    when (isFlagSet flagPaletteIncrement ocps) $ writePort portOCPS ((ocps .&. 0xBF) + 1)
  portVBK <- cgbOnlyPort modeRef 0x00 0x01
    $ \_ vbk' -> vbk' <$ setVRAMBank vram (if vbk' .&. 1 == 0 then 0 else 0x2000)

  assemblySpace  <- VUM.replicate 168 (0, (0, 0, False))
  priorityBuffer <- VUM.replicate 168 0

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
  bufferAvailable <- newEmptyMVar
  signalWindow    <- newEmptyMVar
  pure GraphicsSync { .. }

-- | LCDC flags
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

-- | OAM flags
flagBank, flagOAMPalette, flagHorizontalFlip, flagVerticalFlip, flagDisplayPriority :: Word8
flagBank = 0x08
flagOAMPalette = 0x10
flagHorizontalFlip = 0x20
flagVerticalFlip = 0x40
flagDisplayPriority = 0x80

-- | Palette register flags
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
graphicsStep graphicsState@GraphicsState {..} graphicsSync clockAdvance = do
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

        when (mode' == ReadVRAM) $ setVRAMAccessible vram False

        -- Raise interrupts
        when (stat `testBit` interruptHBlank && mode' == HBlank)
             (raiseInterrupt portIF InterruptLCDCStat)
        when (stat `testBit` interruptVBlank && mode' == VBlank)
             (raiseInterrupt portIF InterruptLCDCStat)
        when (stat `testBit` interruptOAM && mode' == ScanOAM)
             (raiseInterrupt portIF InterruptLCDCStat)
        when (mode' == VBlank) (raiseInterrupt portIF InterruptVBlank)

        when (mode' == HBlank) $ do
          let outputBase = frameBufferBytes `plusPtr` (fromIntegral line' * 640)
          emulatorMode <- readIORef modeRef
          renderLine graphicsState emulatorMode line' outputBase
          setVRAMAccessible vram True

        when (mode' == VBlank) $ do
          putMVar (signalWindow graphicsSync) ()
          takeMVar (bufferAvailable graphicsSync)

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
    let gmode = case stat .&. 0x03 of
          0 -> "HBlank"
          1 -> "VBlank"
          2 -> "Scanning OAM"
          3 -> "Reading VRAM"
          x -> error ("Impossible stat mode " <> show x)
    in  [ ("Mode Flag"                , gmode)
        , ("LYC = LY"                 , show $ stat `testBit` matchBit)
        , ("Interrupt on HBlank", show $ stat `testBit` interruptHBlank)
        , ("Interrupt on VBlank", show $ stat `testBit` interruptVBlank)
        , ("Interrupt on Scanning OAM", show $ stat `testBit` interruptOAM)
        , ("Interrupt on LYC = LY", show $ stat `testBit` interruptCoincidence)
        ]

dmgBackgroundTileAttrs :: Word8
dmgBackgroundTileAttrs = 0

{-# INLINE getBackgroundBlendInfo #-}
getBackgroundBlendInfo :: Word8 -> (Layer, Palette, BGPriority)
getBackgroundBlendInfo attrs =
  let cgbPalette = (attrs .&. 7) .<<. 2 in (0, cgbPalette, isFlagSet flagDisplayPriority attrs)

{-# INLINE getSpriteBlendInfo #-}
getSpriteBlendInfo :: Word8 -> (Layer, Palette, BGPriority)
getSpriteBlendInfo attrs =
  let layer      = if isFlagSet flagOAMPalette attrs then 2 else 1
      cgbPalette = (attrs .&. 7) .<<. 2
  in  (layer, cgbPalette, isFlagSet flagDisplayPriority attrs)

renderLine :: GraphicsState -> EmulatorMode -> Word8 -> Ptr Word8 -> IO ()
renderLine GraphicsState {..} mode line outputBase = do
  scx  <- readPort portSCX
  scy  <- readPort portSCY
  wx   <- readPort portWX
  wy   <- readPort portWY
  lcdc <- readPort portLCDC

  let bgEnabled      = isFlagSet flagBackgroundEnable lcdc
  let windowEnabled  = isFlagSet flagWindowEnable lcdc && line >= wy && wx <= 166
  let spritesEnabled = isFlagSet flagOBJEnable lcdc
  let fontOffset     = if isFlagSet flagTileDataSelect lcdc then 0 else 0x1000
  let windowStart    = if windowEnabled then fromIntegral wx - 7 else 160

  -- Render window data to the assembly area.
  when bgEnabled $ do
    let yTile      = fromIntegral $ (line + scy) .>>. 3
    let yOffset    = fromIntegral $ (line + scy) .&. 0x07
    let xTile      = fromIntegral $ scx .>>. 3
    let xOffset    = fromIntegral $ scx .&. 0x07

    let bgTiles = if isFlagSet flagBackgroundTileMap lcdc then 0x1C00 else 0x1800
    let bgTileLine = bgTiles + (32 * yTile)
    tileMachine bgTileLine fontOffset yOffset windowStart xTile (negate xOffset)

  -- Render window data to the assembly area.
  when windowEnabled $ do
    let yTileWindow    = fromIntegral $ (line - wy) .>>. 3
    let yOffsetWindow  = fromIntegral $ (line - wy) .&. 0x07

    let windowTiles    = if isFlagSet flagWindowTileMap lcdc then 0x1C00 else 0x1800
    let windowTileLine = windowTiles + (32 * yTileWindow)
    tileMachine windowTileLine fontOffset yOffsetWindow 160 0 windowStart

  -- Render sprite data to the assembly area.
  let spriteHeight = if isFlagSet flagOBJSize lcdc then 16 else 8
  when spritesEnabled $ doSprites spriteHeight

  -- Copy the data from the assembly area into the frame texture buffer,
  -- applying the GB palettes.
  if mode == CGB then applyCGBPalettes else applyDMGPalettes

 where
  -- Write tiles to the assembly area. Read from the line of tiles starting at
  -- tileBase, get the pixel data from fontOffset, output pixels from tile row
  -- yOffset, and stop rendering when the output offset is stopOffset.
  -- 
  -- Each round takes the offset to the tile to render, and the x position to
  -- render the pixels to.
  tileMachine !tileBase !fontOffset !yOffset !stopOffset = go
   where
    go !inPos !outPos = if outPos >= stopOffset
      then pure ()
      else do
        tile      <- readTile vram tileBase inPos
        tileAttrs <- if mode == DMG
          then pure dmgBackgroundTileAttrs
          else readTileAttrs vram tileBase inPos
        let hflip        = isFlagSet flagHorizontalFlip tileAttrs
        let vflip        = isFlagSet flagVerticalFlip tileAttrs
        let tileDataBase = if tile > 127 then 0 else fontOffset
        let fontLineOffset = if vflip
              then (fromIntegral tile * 16) + ((7 - yOffset) * 2)
              else (fromIntegral tile * 16) + (yOffset * 2)
        (byte0, byte1) <- if isFlagSet flagBank tileAttrs
          then readBankedTileData vram (tileDataBase + fontLineOffset)
          else readTileData vram (tileDataBase + fontLineOffset)
        outPos' <- pixelMachine byte0 byte1 (getBackgroundBlendInfo tileAttrs) hflip 7 outPos
        go ((inPos + 1) .&. 0x1F) outPos'  -- wrap inPos back to 0 when it gets to the end of the line

  {-# INLINE decodePixel #-}
  decodePixel byte0 byte1 offset =
    0x03 .&. (((byte0 .>>. offset) .&. 0x01) .|. ((byte1 .>>. offset) .<<. 1))

  -- Write pixels to the assembly area. Takes two bytes containing the pixel
  -- data, the bit offset into the bytes, and the x position to output the
  -- pixels to. Returns the final x position. Negative values for pixelOffset
  -- are acceptable, pixelMachine will skip pixels that were scheduled to go to
  -- negative positions.
  pixelMachine !byte0 !byte1 !blendInfo !hflip = go
   where
    go !byteOffset !pixelOffset = if pixelOffset < 0
      then go (byteOffset - 1) (pixelOffset + 1)
      else do
        let pixel = decodePixel byte0 byte1 (if hflip then 7 - byteOffset else byteOffset)
        VUM.unsafeWrite assemblySpace pixelOffset (pixel, blendInfo)
        if byteOffset == 0 then pure (pixelOffset + 1) else go (byteOffset - 1) (pixelOffset + 1)

  -- Write sprites to the assembly area.
  doSprites !h = go 0 0
   where
    go !offset !spritesRendered = if offset >= 40 || spritesRendered >= (10 :: Int)
      then pure ()
      else do
        spriteRendered <- doSprite h (offset * 4)
        go (offset + 1) (if spriteRendered then spritesRendered + 1 else spritesRendered)

  doSprite !h !offset = do
    (y, x) <- readSpritePosition vram offset
    let spriteVisible = x /= 0 && line + 16 >= y && line + 16 < y + h
    if not spriteVisible
      then pure False
      else do
        when (x < 168) $ do
          (rawCode, attrs) <- readSpriteAttributes vram offset
          let code  = if h == 16 then rawCode .&. 0xFE else rawCode
          let vflip = isFlagSet flagVerticalFlip attrs
          let hflip = isFlagSet flagHorizontalFlip attrs
          let fontLineOffset = (16 * fromIntegral code) + 2 * fromIntegral
                (if vflip then y + h - 17 - line else line + 16 - y)
          let blendInfo = getSpriteBlendInfo attrs
          let xOffset   = fromIntegral x - 8
          (byte0, byte1) <- if mode == CGB && isFlagSet flagBank attrs
            then readBankedTileData vram fontLineOffset
            else readTileData vram fontLineOffset

          let priority = if mode == CGB then 0 else fromIntegral xOffset
          blendSprite priority hflip blendInfo byte0 byte1 xOffset
        pure True

  blendSprite !priority !hflip blendInfo@(_, _, blendBgPriority) !byte0 !byte1 !outOffset = go 0
   where
    go 8  = pure ()
    go !i = do
      let pixel = decodePixel byte0 byte1 (if hflip then i else 7 - i)
      let outPos = outOffset + i
      when (pixel /= 0 && outPos >= 0) $ do
        (previousPixel, (previousLayer, _, previousBgPriority)) <- VUM.unsafeRead assemblySpace
                                                                                  outPos
        previousPriority <- VUM.unsafeRead priorityBuffer outPos
        let drawOverSprite = previousLayer /= 0
        let bgPriority     = blendBgPriority || previousBgPriority
        when
            (  (drawOverSprite && priority < previousPriority)
            || (not drawOverSprite && (not bgPriority || (bgPriority && (previousPixel == 0))))
            )
          $ do
              VUM.unsafeWrite assemblySpace outPos (pixel, blendInfo)
              VUM.unsafeWrite priorityBuffer outPos priority
      go (i + 1)

  -- Generate actual pixel data in the frame texture buffer based on the data in
  -- the assembly area.
  applyDMGPalettes = do
    bgp  <- readPort portBGP
    obp0 <- readPort portOBP0
    obp1 <- readPort portOBP1

    let
      go !offset = do
        (index, (layer, _, _)) <- VUM.unsafeRead assemblySpace offset
        let paletteData = case layer of
              0 -> bgp :: Word8
              1 -> obp0
              2 -> obp1
              x -> error ("Framebuffer corrupted " <> show x <> " at " <> show offset)
        let mappedIndex = (paletteData .>>. (fromIntegral index * 2)) .&. 0x03
        color <- readRGBPalette vram
                                (layer > 0)
                                (if layer == 2 then 4 .|. mappedIndex else mappedIndex)
        pokeElemOff (castPtr outputBase) offset color
        if offset >= 159 then pure () else go (offset + 1)

    go 0

  applyCGBPalettes = go 0
   where
    go !offset = do
      (index, (layer, palette, _)) <- VUM.unsafeRead assemblySpace offset
      color                        <- readRGBPalette vram (layer > 0) (index .|. palette)
      pokeElemOff (castPtr outputBase) offset color
      if offset >= 159 then pure () else go (offset + 1)
