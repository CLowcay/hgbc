{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.Graphics
  ( State (..),
    Mode (..),
    BusEvent (..),
    Sync (..),
    newSync,
    init,
    ports,
    step,
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Reader (when)
import Data.Bits (Bits (complement, testBit, (.&.), (.|.)))
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int8)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word16, Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (pokeElemOff))
import qualified Machine.GBC.CPU.Interrupts as Interrupt
import Machine.GBC.Graphics.VRAM (VRAM)
import qualified Machine.GBC.Graphics.VRAM as VRAM
import Machine.GBC.Mode (EmulatorMode (..), cgbOnlyPort)
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port
import Machine.GBC.Primitive.StateCycle (StateCycle)
import qualified Machine.GBC.Primitive.StateCycle as StateCycle
import qualified Machine.GBC.Registers as R
import Machine.GBC.Util (isFlagSet, (.<<.), (.>>.))
import Prelude hiding (init)

-- | The current status of the graphics system.
data Mode = HBlank | VBlank | ScanOAM | ReadVRAM
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | The graphics state.
data State = State
  { lcdState :: !(StateCycle Mode),
    lcdLine :: !(StateCycle Word8),
    statSignal :: !(IORef Bool),
    portLCDC :: !Port,
    portSTAT :: !Port,
    portSCY :: !Port,
    portSCX :: !Port,
    portLY :: !Port,
    portLYC :: !Port,
    portBGP :: !Port,
    portOBP0 :: !Port,
    portOBP1 :: !Port,
    portWY :: !Port,
    portWX :: !Port,
    portBCPS :: !Port,
    portBCPD :: !Port,
    portOCPS :: !Port,
    portOCPD :: !Port,
    portVBK :: !Port,
    portIF :: !Port,
    vram :: !VRAM,
    modeRef :: !(IORef EmulatorMode),
    frameBufferBytes :: !(Ptr Word8),
    -- | A temporary area for blending the background, window, and sprites.
    assemblySpace :: !(VUM.IOVector PixelInfo),
    -- | Each byte in this buffer corresponds to one of the pixels in the
    -- lineAssembly space. When a sprite pixel is drawn, the x-position of that
    -- sprite is written to the spritePriorityBuffer. This allows us to simulate
    -- the DMG behavior where sprites with lower x-positions take priority.
    priorityBuffer :: !(VUM.IOVector Int8)
  }

type Index = Word8 -- Values ranging from 0 ~ 3

type Palette = Word8 -- Values ranging from 0 ~ 7, only valid for CGB.

type Layer = Word8 -- 0, 1, or 2.  0 for background, 1 for sprite layer 1, 2 for sprite layer 2.

type BGPriority = Bool -- True if the background layer has priority.

type PixelInfo = (Index, (Layer, Palette, BGPriority))

-- | Graphics synchronization objects. The output thread should wait on
-- signalWindow, then draw the buffer, then put to bufferAvailable when it is
-- safe to write to the frame buffer again.
data Sync = Sync
  { -- | The output window puts a () here when it is safe to write to the frame buffer again.
    bufferAvailable :: !(MVar ()),
    -- | Wait on this MVar before drawing the output buffer.
    signalWindow :: !(MVar ())
  }

lcdStates :: [(Mode, Int)]
lcdStates =
  concat (replicate 144 [(ScanOAM, 80), (ReadVRAM, 172), (HBlank, 204)]) ++ [(VBlank, 4560)]

lcdLines :: [(Word8, Int)]
lcdLines = [0 .. 153] <&> (,456)

-- | The initial graphics state.
init :: VRAM -> IORef EmulatorMode -> Ptr Word8 -> Port -> IO State
init vram modeRef frameBufferBytes portIF = mdo
  lcdState <- StateCycle.new lcdStates
  lcdLine <- StateCycle.new lcdLines
  statSignal <- newIORef True

  portLCDC <- Port.new 0xFF 0xFF $ \lcdc lcdc' -> do
    let lcdEnabled = isFlagSet flagLCDEnable lcdc
    let lcdEnabled' = isFlagSet flagLCDEnable lcdc'
    when (lcdEnabled' && not lcdEnabled) $ do
      StateCycle.reset lcdLine lcdLines
      StateCycle.reset lcdState lcdStates
      Port.writeDirect portLY 0
      stat <- Port.readDirect portSTAT
      Port.writeDirect portSTAT (modifyBits maskMode stat 1)
      ly <- Port.readDirect portLY
      lyc <- Port.readDirect portLYC
      checkStatInterrupt portIF portSTAT statSignal ly lyc =<< StateCycle.getState lcdState
    when (not lcdEnabled' && lcdEnabled) $ do
      stat <- Port.readDirect portSTAT
      Port.writeDirect portSTAT (stat .&. 0xFC)
      Port.writeDirect portLY 0
    pure lcdc'
  portSTAT <- Port.new 0x80 0x78 Port.alwaysUpdate
  portSCY <- Port.new 0x00 0xFF Port.alwaysUpdate
  portSCX <- Port.new 0x00 0xFF Port.alwaysUpdate
  portLY <- Port.new 0x00 0x00 Port.neverUpdate
  portLYC <- Port.new 0x00 0xFF $ \_ lyc -> do
    ly <- Port.readDirect portLY
    checkStatInterrupt portIF portSTAT statSignal ly lyc =<< StateCycle.getState lcdState
    pure lyc
  portBGP <- Port.new 0xFF 0xFF Port.alwaysUpdate
  portOBP0 <- Port.new 0xFF 0xFF Port.alwaysUpdate
  portOBP1 <- Port.new 0xFF 0xFF Port.alwaysUpdate
  portWY <- Port.new 0x00 0xFF Port.alwaysUpdate
  portWX <- Port.new 0x00 0xFF Port.alwaysUpdate
  portBCPS <- Port.new 0x40 0xBF $
    \_ bcps' -> bcps' <$ (Port.writeDirect portBCPD =<< VRAM.readPalette vram False bcps')
  portBCPD <- cgbOnlyPort modeRef 0x00 0xFF $ \_ bcpd' ->
    bcpd' <$ do
      bcps <- Port.read portBCPS
      VRAM.writePalette vram False bcps bcpd'
      when (isFlagSet flagPaletteIncrement bcps) $ Port.write portBCPS ((bcps .&. 0xBF) + 1)
  portOCPS <- Port.new 0x40 0xBF $
    \_ ocps' -> ocps' <$ (Port.writeDirect portOCPD =<< VRAM.readPalette vram True ocps')
  portOCPD <- cgbOnlyPort modeRef 0x00 0xFF $ \_ ocpd' ->
    ocpd' <$ do
      ocps <- Port.read portOCPS
      VRAM.writePalette vram True ocps ocpd'
      when (isFlagSet flagPaletteIncrement ocps) $ Port.write portOCPS ((ocps .&. 0xBF) + 1)
  portVBK <- Port.new 0xFE 0x01 $
    \_ vbk' -> vbk' <$ VRAM.setBank vram (if vbk' .&. 1 == 0 then 0 else 0x2000)

  assemblySpace <- VUM.replicate 168 (0, (0, 0, False))
  priorityBuffer <- VUM.replicate 168 0

  pure State {..}

ports :: State -> [(Word16, Port)]
ports State {..} =
  [ (R.LCDC, portLCDC),
    (R.STAT, portSTAT),
    (R.SCY, portSCY),
    (R.SCX, portSCX),
    (R.LY, portLY),
    (R.LYC, portLYC),
    (R.BGP, portBGP),
    (R.OBP0, portOBP0),
    (R.OBP1, portOBP1),
    (R.WY, portWY),
    (R.WX, portWX),
    (R.BCPS, portBCPS),
    (R.BCPD, portBCPD),
    (R.OCPS, portOCPS),
    (R.OCPD, portOCPD),
    (R.VBK, portVBK)
  ]

-- | Make a new Graphics sync object.
newSync :: IO Sync
newSync = do
  bufferAvailable <- newEmptyMVar
  signalWindow <- newEmptyMVar
  pure Sync {..}

-- | LCDC flags
flagLCDEnable,
  flagWindowTileMap,
  flagWindowEnable,
  flagTileDataSelect,
  flagBackgroundTileMap,
  flagOBJSize,
  flagOBJEnable,
  flagBackgroundEnable ::
    Word8
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
modeBits HBlank = 0
modeBits VBlank = 1
modeBits ScanOAM = 2
modeBits ReadVRAM = 3

checkLY :: Port -> Word8 -> Word8 -> IO Word8
checkLY portSTAT ly lyc = do
  stat <- Port.readDirect portSTAT
  let stat' = (stat .&. complement matchMask) .|. if ly == lyc then matchMask else 0
  stat' <$ Port.writeDirect portSTAT stat'

updateStatSignal :: IORef Bool -> Mode -> Word8 -> IO Bool
updateStatSignal signalRef mode stat = do
  signal <- readIORef signalRef
  let signal' =
        (stat .&. conicidenceMask == conicidenceMask) || case mode of
          HBlank -> stat `testBit` interruptHBlank
          VBlank -> stat .&. interruptVBlankOrOAM /= 0
          ScanOAM -> stat `testBit` interruptOAM
          ReadVRAM -> False
  writeIORef signalRef signal'
  pure (signal' && not signal)

checkStatInterrupt :: Port -> Port -> IORef Bool -> Word8 -> Word8 -> Mode -> IO ()
checkStatInterrupt portIF portSTAT signalRef ly lyc mode = do
  raise <- updateStatSignal signalRef mode =<< checkLY portSTAT ly lyc
  when raise $ Interrupt.raise portIF Interrupt.LCDCStat

data BusEvent = NoGraphicsEvent | HBlankEvent deriving (Eq, Ord, Show)

{-# INLINEABLE step #-}
step :: State -> Sync -> Int -> IO BusEvent
step graphicsState@State {..} graphicsSync clockAdvance = do
  lcdc <- Port.readDirect portLCDC
  let lcdEnabled = isFlagSet flagLCDEnable lcdc
  if not lcdEnabled
    then pure NoGraphicsEvent
    else do
      line' <- StateCycle.getUpdateResult <$> StateCycle.update lcdLine clockAdvance (Port.writeDirect portLY)
      modeUpdate <- StateCycle.update lcdState clockAdvance $ \mode' -> do
        -- Update STAT register
        stat <- Port.readDirect portSTAT
        Port.writeDirect portSTAT (modifyBits maskMode (modeBits mode') stat)

        when (mode' == ScanOAM) $ VRAM.setOAMAccessible vram False
        when (mode' == ReadVRAM) $ VRAM.setAccessible vram False

        -- Raise interrupts
        lyc <- Port.readDirect portLYC
        checkStatInterrupt portIF portSTAT statSignal line' lyc mode'
        when (mode' == VBlank) $ Interrupt.raise portIF Interrupt.VBlank

        when (mode' == HBlank) $ do
          let outputBase = frameBufferBytes `plusPtr` (fromIntegral line' * 640)
          emulatorMode <- readIORef modeRef
          renderLine graphicsState emulatorMode line' outputBase
          VRAM.setOAMAccessible vram True
          VRAM.setAccessible vram True

        when (mode' == VBlank) $ do
          putMVar (signalWindow graphicsSync) ()
          takeMVar (bufferAvailable graphicsSync)

      pure $ case modeUpdate of
        StateCycle.HasChangedTo HBlank -> HBlankEvent
        _ -> NoGraphicsEvent

dmgBackgroundTileAttrs :: Word8
dmgBackgroundTileAttrs = 0

{-# INLINE getBackgroundBlendInfo #-}
getBackgroundBlendInfo :: Word8 -> (Layer, Palette, BGPriority)
getBackgroundBlendInfo attrs =
  let cgbPalette = (attrs .&. 7) .<<. 2 in (0, cgbPalette, isFlagSet flagDisplayPriority attrs)

{-# INLINE getSpriteBlendInfo #-}
getSpriteBlendInfo :: Word8 -> (Layer, Palette, BGPriority)
getSpriteBlendInfo attrs =
  let layer = if isFlagSet flagOAMPalette attrs then 2 else 1
      cgbPalette = (attrs .&. 7) .<<. 2
   in (layer, cgbPalette, isFlagSet flagDisplayPriority attrs)

renderLine :: State -> EmulatorMode -> Word8 -> Ptr Word8 -> IO ()
renderLine State {..} mode line outputBase = do
  scx <- Port.readDirect portSCX
  scy <- Port.readDirect portSCY
  wx <- Port.readDirect portWX
  wy <- Port.readDirect portWY
  lcdc <- Port.readDirect portLCDC

  let bgEnabled = isFlagSet flagBackgroundEnable lcdc
  let windowEnabled = isFlagSet flagWindowEnable lcdc && line >= wy && wx <= 166
  let spritesEnabled = isFlagSet flagOBJEnable lcdc
  let fontOffset = if isFlagSet flagTileDataSelect lcdc then 0 else 0x1000
  let windowStart = if windowEnabled then fromIntegral wx - 7 else 160

  -- Render window data to the assembly area.
  when bgEnabled $ do
    let yTile = fromIntegral $ (line + scy) .>>. 3
    let yOffset = fromIntegral $ (line + scy) .&. 0x07
    let xTile = fromIntegral $ scx .>>. 3
    let xOffset = fromIntegral $ scx .&. 0x07

    let bgTiles = if isFlagSet flagBackgroundTileMap lcdc then 0x1C00 else 0x1800
    let bgTileLine = bgTiles + (32 * yTile)
    tileMachine bgTileLine fontOffset yOffset windowStart xTile (negate xOffset)

  -- Render window data to the assembly area.
  when windowEnabled $ do
    let yTileWindow = fromIntegral $ (line - wy) .>>. 3
    let yOffsetWindow = fromIntegral $ (line - wy) .&. 0x07

    let windowTiles = if isFlagSet flagWindowTileMap lcdc then 0x1C00 else 0x1800
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
        go !inPos !outPos =
          if outPos >= stopOffset
            then pure ()
            else do
              tile <- VRAM.readTile vram tileBase inPos
              tileAttrs <-
                if mode == DMG
                  then pure dmgBackgroundTileAttrs
                  else VRAM.readTileAttrs vram tileBase inPos
              let hflip = isFlagSet flagHorizontalFlip tileAttrs
              let vflip = isFlagSet flagVerticalFlip tileAttrs
              let tileDataBase = if tile > 127 then 0 else fontOffset
              let fontLineOffset =
                    if vflip
                      then (fromIntegral tile * 16) + ((7 - yOffset) * 2)
                      else (fromIntegral tile * 16) + (yOffset * 2)
              (byte0, byte1) <-
                if isFlagSet flagBank tileAttrs
                  then VRAM.readBankedTileData vram (tileDataBase + fontLineOffset)
                  else VRAM.readTileData vram (tileDataBase + fontLineOffset)
              outPos' <- pixelMachine byte0 byte1 (getBackgroundBlendInfo tileAttrs) hflip 7 outPos
              go ((inPos + 1) .&. 0x1F) outPos' -- wrap inPos back to 0 when it gets to the end of the line
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
        go !byteOffset !pixelOffset =
          if pixelOffset < 0
            then go (byteOffset - 1) (pixelOffset + 1)
            else do
              let pixel = decodePixel byte0 byte1 (if hflip then 7 - byteOffset else byteOffset)
              VUM.unsafeWrite assemblySpace pixelOffset (pixel, blendInfo)
              if byteOffset == 0 then pure (pixelOffset + 1) else go (byteOffset - 1) (pixelOffset + 1)

    -- Write sprites to the assembly area.
    doSprites !h = go 0 0
      where
        go !offset !spritesRendered =
          if offset >= 40 || spritesRendered >= (10 :: Int)
            then pure ()
            else do
              spriteRendered <- doSprite h (offset * 4)
              go (offset + 1) (if spriteRendered then spritesRendered + 1 else spritesRendered)

    doSprite !h !offset = do
      (y, x) <- VRAM.readSpritePosition vram offset
      let spriteVisible = x /= 0 && line + 16 >= y && line + 16 < y + h
      if not spriteVisible
        then pure False
        else do
          when (x < 168) $ do
            (rawCode, attrs) <- VRAM.readSpriteAttributes vram offset
            let code = if h == 16 then rawCode .&. 0xFE else rawCode
            let vflip = isFlagSet flagVerticalFlip attrs
            let hflip = isFlagSet flagHorizontalFlip attrs
            let fontLineOffset =
                  (16 * fromIntegral code) + 2
                    * fromIntegral
                      (if vflip then y + h - 17 - line else line + 16 - y)
            let blendInfo = getSpriteBlendInfo attrs
            let xOffset = fromIntegral x - 8
            (byte0, byte1) <-
              if mode == CGB && isFlagSet flagBank attrs
                then VRAM.readBankedTileData vram fontLineOffset
                else VRAM.readTileData vram fontLineOffset

            let priority = if mode == CGB then 0 else fromIntegral xOffset
            blendSprite priority hflip blendInfo byte0 byte1 xOffset
          pure True

    blendSprite !priority !hflip blendInfo@(_, _, blendBgPriority) !byte0 !byte1 !outOffset = go 0
      where
        go 8 = pure ()
        go !i = do
          let pixel = decodePixel byte0 byte1 (if hflip then i else 7 - i)
          let outPos = outOffset + i
          when (pixel /= 0 && outPos >= 0) $ do
            (previousPixel, (previousLayer, _, previousBgPriority)) <-
              VUM.unsafeRead
                assemblySpace
                outPos
            previousPriority <- VUM.unsafeRead priorityBuffer outPos
            let drawOverSprite = previousLayer /= 0
            let bgPriority = blendBgPriority || previousBgPriority
            when
              ( (drawOverSprite && priority < previousPriority)
                  || (not drawOverSprite && (not bgPriority || (bgPriority && (previousPixel == 0))))
              )
              $ do
                VUM.unsafeWrite assemblySpace outPos (pixel, blendInfo)
                VUM.unsafeWrite priorityBuffer outPos priority
          go (i + 1)

    -- Generate actual pixel data in the frame texture buffer based on the data in
    -- the assembly area.
    applyDMGPalettes = do
      bgp <- Port.readDirect portBGP
      obp0 <- Port.readDirect portOBP0
      obp1 <- Port.readDirect portOBP1

      let go !offset = do
            (index, (layer, _, _)) <- VUM.unsafeRead assemblySpace offset
            let paletteData = case layer of
                  0 -> bgp :: Word8
                  1 -> obp0
                  2 -> obp1
                  x -> error ("Framebuffer corrupted " <> show x <> " at " <> show offset)
            let mappedIndex = (paletteData .>>. (fromIntegral index * 2)) .&. 0x03
            color <-
              VRAM.readRGBPalette
                vram
                (layer > 0)
                (if layer == 2 then 4 .|. mappedIndex else mappedIndex)
            pokeElemOff (castPtr outputBase) offset color
            if offset >= 159 then pure () else go (offset + 1)

      go 0

    applyCGBPalettes = go 0
      where
        go !offset = do
          (index, (layer, palette, _)) <- VUM.unsafeRead assemblySpace offset
          color <- VRAM.readRGBPalette vram (layer > 0) (index .|. palette)
          pokeElemOff (castPtr outputBase) offset color
          if offset >= 159 then pure () else go (offset + 1)
