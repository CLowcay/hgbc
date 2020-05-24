{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Machine.GBC.Graphics
  ( State(..)
  , Mode(..)
  , BusEvent(..)
  , Sync(..)
  , newSync
  , init
  , ports
  , step
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
import           Prelude                 hiding ( init )
import qualified Data.Vector.Unboxed.Mutable   as VUM

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data State = State {
    lcdState      :: !(StateCycle Mode)
  , lcdLine       :: !(StateCycle Word8)
  , statSignal    :: !(IORef Bool)
  , portLCDC      :: !Port
  , portSTAT      :: !Port
  , portSCY       :: !Port
  , portSCX       :: !Port
  , portLY        :: !Port
  , portLYC       :: !Port
  , portBGP       :: !Port
  , portOBP0      :: !Port
  , portOBP1      :: !Port
  , portWY        :: !Port
  , portWX        :: !Port
  , portBCPS      :: !Port
  , portBCPD      :: !Port
  , portOCPS      :: !Port
  , portOCPD      :: !Port
  , portVBK       :: !Port
  , portIF        :: !Port
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
data Sync = Sync {
    bufferAvailable :: !(MVar ())    -- ^ The output window puts a () here when it is safe to write to the frame buffer again.
  , signalWindow    :: !(MVar ())    -- ^ Wait on this MVar before drawing the output buffer.
}

lcdStates :: [(Mode, Int)]
lcdStates =
  concat (replicate 144 [(ScanOAM, 80), (ReadVRAM, 172), (HBlank, 204)]) ++ [(VBlank, 4560)]

lcdLines :: [(Word8, Int)]
lcdLines = [0 .. 153] <&> (, 456)

-- | The initial graphics state.
init :: VRAM -> IORef EmulatorMode -> Ptr Word8 -> Port -> IO State
init vram modeRef frameBufferBytes portIF = mdo
  lcdState   <- newStateCycle lcdStates
  lcdLine    <- newStateCycle lcdLines
  statSignal <- newIORef True

  portLCDC   <- newPort 0xFF 0xFF $ \lcdc lcdc' -> do
    let lcdEnabled  = isFlagSet flagLCDEnable lcdc
    let lcdEnabled' = isFlagSet flagLCDEnable lcdc'
    when (lcdEnabled' && not lcdEnabled) $ do
      resetStateCycle lcdLine  lcdLines
      resetStateCycle lcdState lcdStates
      directWritePort portLY 0
      stat <- directReadPort portSTAT
      directWritePort portSTAT (modifyBits maskMode stat 1)
      ly  <- directReadPort portLY
      lyc <- directReadPort portLYC
      checkStatInterrupt portIF portSTAT statSignal ly lyc =<< getStateCycle lcdState
    when (not lcdEnabled' && lcdEnabled) $ do
      stat <- directReadPort portSTAT
      directWritePort portSTAT (stat .&. 0xFC)
      directWritePort portLY   0
    pure lcdc'
  portSTAT <- newPort 0x80 0x78 alwaysUpdate
  portSCY  <- newPort 0x00 0xFF alwaysUpdate
  portSCX  <- newPort 0x00 0xFF alwaysUpdate
  portLY   <- newPort 0x00 0x00 neverUpdate
  portLYC  <- newPort 0x00 0xFF $ \_ lyc -> do
    ly <- directReadPort portLY
    checkStatInterrupt portIF portSTAT statSignal ly lyc =<< getStateCycle lcdState
    pure lyc
  portBGP  <- newPort 0xFF 0xFF alwaysUpdate
  portOBP0 <- newPort 0xFF 0xFF alwaysUpdate
  portOBP1 <- newPort 0xFF 0xFF alwaysUpdate
  portWY   <- newPort 0x00 0xFF alwaysUpdate
  portWX   <- newPort 0x00 0xFF alwaysUpdate
  portBCPS <- newPort 0x40 0xBF
    $ \_ bcps' -> bcps' <$ (directWritePort portBCPD =<< readPalette vram False bcps')
  portBCPD <- cgbOnlyPort modeRef 0x00 0xFF $ \_ bcpd' -> bcpd' <$ do
    bcps <- readPort portBCPS
    writePalette vram False bcps bcpd'
    when (isFlagSet flagPaletteIncrement bcps) $ writePort portBCPS ((bcps .&. 0xBF) + 1)
  portOCPS <- newPort 0x40 0xBF
    $ \_ ocps' -> ocps' <$ (directWritePort portOCPD =<< readPalette vram True ocps')
  portOCPD <- cgbOnlyPort modeRef 0x00 0xFF $ \_ ocpd' -> ocpd' <$ do
    ocps <- readPort portOCPS
    writePalette vram True ocps ocpd'
    when (isFlagSet flagPaletteIncrement ocps) $ writePort portOCPS ((ocps .&. 0xBF) + 1)
  portVBK <- newPort 0xFE 0x01
    $ \_ vbk' -> vbk' <$ setVRAMBank vram (if vbk' .&. 1 == 0 then 0 else 0x2000)

  assemblySpace  <- VUM.replicate 168 (0, (0, 0, False))
  priorityBuffer <- VUM.replicate 168 0

  pure State { .. }

ports :: State -> [(Word16, Port)]
ports State {..} =
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
newSync :: IO Sync
newSync = do
  bufferAvailable <- newEmptyMVar
  signalWindow    <- newEmptyMVar
  pure Sync { .. }

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

interruptOAM, interruptHBlank :: Int
interruptOAM = 5
interruptHBlank = 3

matchMask, interruptVBlankOrOAM, conicidenceMask :: Word8
matchMask = 0x04
interruptVBlankOrOAM = 0x30
conicidenceMask = 0x44

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

checkLY :: Port -> Word8 -> Word8 -> IO Word8
checkLY portSTAT ly lyc = do
  stat <- directReadPort portSTAT
  let stat' = (stat .&. complement matchMask) .|. if ly == lyc then matchMask else 0
  stat' <$ directWritePort portSTAT stat'

updateStatSignal :: IORef Bool -> Mode -> Word8 -> IO Bool
updateStatSignal signalRef mode stat = do
  signal <- readIORef signalRef
  let signal' = (stat .&. conicidenceMask == conicidenceMask) || case mode of
        HBlank   -> stat `testBit` interruptHBlank
        VBlank   -> stat .&. interruptVBlankOrOAM /= 0
        ScanOAM  -> stat `testBit` interruptOAM
        ReadVRAM -> False
  writeIORef signalRef signal'
  pure (signal' && not signal)

checkStatInterrupt :: Port -> Port -> IORef Bool -> Word8 -> Word8 -> Mode -> IO ()
checkStatInterrupt portIF portSTAT signalRef ly lyc mode = do
  raise <- updateStatSignal signalRef mode =<< checkLY portSTAT ly lyc
  when raise $ raiseInterrupt portIF InterruptLCDCStat

data BusEvent = NoGraphicsEvent | HBlankEvent deriving (Eq, Ord, Show)

{-# INLINABLE step #-}
step :: State -> Sync -> Int -> IO BusEvent
step graphicsState@State {..} graphicsSync clockAdvance = do
  lcdc <- directReadPort portLCDC
  let lcdEnabled = isFlagSet flagLCDEnable lcdc
  if not lcdEnabled
    then pure NoGraphicsEvent
    else do
      line' <- getUpdateResult <$> updateStateCycle lcdLine clockAdvance (directWritePort portLY)
      modeUpdate <- updateStateCycle lcdState clockAdvance $ \mode' -> do
        -- Update STAT register
        stat <- directReadPort portSTAT
        directWritePort portSTAT (modifyBits maskMode (modeBits mode') stat)

        when (mode' == ScanOAM) $ setOAMAccessible vram False
        when (mode' == ReadVRAM) $ setVRAMAccessible vram False

        -- Raise interrupts
        lyc <- directReadPort portLYC
        checkStatInterrupt portIF portSTAT statSignal line' lyc mode'
        when (mode' == VBlank) $ raiseInterrupt portIF InterruptVBlank

        when (mode' == HBlank) $ do
          let outputBase = frameBufferBytes `plusPtr` (fromIntegral line' * 640)
          emulatorMode <- readIORef modeRef
          renderLine graphicsState emulatorMode line' outputBase
          setOAMAccessible vram True
          setVRAMAccessible vram True

        when (mode' == VBlank) $ do
          putMVar (signalWindow graphicsSync) ()
          takeMVar (bufferAvailable graphicsSync)

      pure $ case modeUpdate of
        HasChangedTo HBlank -> HBlankEvent
        _                   -> NoGraphicsEvent

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

renderLine :: State -> EmulatorMode -> Word8 -> Ptr Word8 -> IO ()
renderLine State {..} mode line outputBase = do
  scx  <- directReadPort portSCX
  scy  <- directReadPort portSCY
  wx   <- directReadPort portWX
  wy   <- directReadPort portWY
  lcdc <- directReadPort portLCDC

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
    bgp  <- directReadPort portBGP
    obp0 <- directReadPort portOBP0
    obp1 <- directReadPort portOBP1

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
