{-# LANGUAGE RecordWildCards #-}
module Machine.GBC.Graphics.VRAM
  ( VRAM
  , ColorCorrection(..)
  , initVRAM
  , setVRAMAccessible
  , setVRAMBank
  , writePalette
  , readPalette
  , readRGBPalette
  , writeRGBPalette
  , readSpritePosition
  , readSpriteAttributes
  , readTile
  , readTileAttrs
  , readTileData
  , readBankedTileData
  , readOAM
  , writeOAM
  , readVRAM
  , writeVRAM
  , copyToOAM
  , copyVRAMToOAM
  )
where

import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.Mode
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.Util
import qualified Data.Vector.Storable.Mutable  as VSM

-- | The color correction scheme to use.
data ColorCorrection
  = NoColorCorrection
  | DefaultColorCorrection
  deriving (Eq, Ord, Show)

type ColorFunction = (Word16, Word16, Word16) -> (Word32, Word32, Word32)

data VRAM = VRAM {
    vram           :: !(VSM.IOVector Word8)
  , oam            :: !(VSM.IOVector Word8)
  , rawPalettes    :: !(VSM.IOVector Word16)
  , rgbPalettes    :: !(VSM.IOVector Word32)
  , mode           :: !EmulatorMode
  , vramAccessible :: !(IORef Bool)
  , vramBank       :: !(UnboxedRef Int)
  , colorFunction  :: !ColorFunction
}

totalPaletteEntries :: Int
totalPaletteEntries = 8 * 4 * 2 -- 2 sets of 8 palettes with 4 colors each.

initVRAM :: EmulatorMode -> ColorCorrection -> IO VRAM
initVRAM mode colorCorrection = do
  let size = case mode of
        DMG -> 0x2000
        CGB -> 0x4000
  vram           <- VSM.new size
  oam            <- VSM.new 160
  vramAccessible <- newIORef True
  vramBank       <- newUnboxedRef 0
  rawPalettes    <- VSM.replicate totalPaletteEntries 0x7FFF
  rgbPalettes    <- VSM.replicate totalPaletteEntries 0xFFFFFFFF
  let colorFunction = case colorCorrection of
        NoColorCorrection      -> vgaColors
        DefaultColorCorrection -> defaultColors

  pure VRAM { .. }

{-# INLINE setVRAMAccessible #-}
setVRAMAccessible :: VRAM -> Bool -> IO ()
setVRAMAccessible VRAM {..} = writeIORef vramAccessible

{-# INLINE setVRAMBank #-}
setVRAMBank :: VRAM -> Int -> IO ()
setVRAMBank VRAM {..} = writeUnboxedRef vramBank

-- | Get the byte offset into palette memory given the value of a cps register.
paletteByte :: Bool -> Word8 -> Int
paletteByte fg addr = (if fg then 64 else 0) + fromIntegral addr .&. 0x3F

-- | Get the index to the color entry of a palette.
paletteIndex :: Bool -> Word8 -> Int
paletteIndex fg addr = (if fg then 8 * 4 else 0) + fromIntegral addr .&. 0x1F

-- | Write a palette given the values of the cps and cpd registers.
writePalette :: VRAM -> Bool -> Word8 -> Word8 -> IO ()
writePalette VRAM {..} fg cps cpd = do
  VSM.unsafeWrite (VSM.unsafeCast rawPalettes) (paletteByte fg cps) cpd
  let i = paletteIndex fg (cps .>>. 1)
  raw <- VSM.unsafeRead rawPalettes i
  VSM.unsafeWrite rgbPalettes i (encodeColor colorFunction raw)

-- | Read a palette given the value of the cps register.
{-# INLINE readPalette #-}
readPalette :: VRAM -> Bool -> Word8 -> IO Word8
readPalette VRAM {..} fg cps = VSM.unsafeRead (VSM.unsafeCast rawPalettes) (paletteByte fg cps)

-- | Read a decoded RGB palette with the given 5-bit address
{-# INLINE readRGBPalette #-}
readRGBPalette :: VRAM -> Bool -> Word8 -> IO Word32
readRGBPalette VRAM {..} fg addr = VSM.unsafeRead rgbPalettes (paletteIndex fg addr)

-- | Write RGB palette data.
writeRGBPalette
  :: VRAM    -- ^ Video RAM.
  -> Bool    -- ^ True to write a foreground palette, otherwise write a background palette.
  -> Int     -- ^ The palette number (0 to 7) to write.
  -> (Word32, Word32, Word32, Word32)
  -> IO ()
writeRGBPalette VRAM {..} fg i (c0, c1, c2, c3) =
  let base = 4 * i + if fg then 32 else 0
  in  do
        VSM.write rgbPalettes base (swizzle c0)
        VSM.write rgbPalettes (base + 1) (swizzle c1)
        VSM.write rgbPalettes (base + 2) (swizzle c2)
        VSM.write rgbPalettes (base + 3) (swizzle c3)
 where
  swizzle x =
    let r = x .>>. 24
        g = (x .>>. 16) .&. 0xFF
        b = (x .>>. 8) .&. 0xFF
        a = x .&. 0xFF
    in  (a .<<. 24) .|. (b .<<. 16) .|. (g .<<. 8) .|. r

{-# INLINE readSpritePosition #-}
readSpritePosition :: VRAM -> Int -> IO (Word8, Word8)
readSpritePosition VRAM {..} i = do
  y <- VSM.unsafeRead oam i
  x <- VSM.unsafeRead oam (i + 1)
  pure (y, x)

{-# INLINE readSpriteAttributes #-}
readSpriteAttributes :: VRAM -> Int -> IO (Word8, Word8)
readSpriteAttributes VRAM {..} i = do
  c <- VSM.unsafeRead oam (i + 2)
  a <- VSM.unsafeRead oam (i + 3)
  pure (c, a)

{-# INLINE readTile #-}
readTile :: VRAM -> Int -> Int -> IO Word8
readTile VRAM {..} area tile = VSM.unsafeRead vram (area + tile)

-- WARNING: DO NOT CALL unless running in DMG mode.
{-# INLINE readTileAttrs #-}
readTileAttrs :: VRAM -> Int -> Int -> IO Word8
readTileAttrs VRAM {..} area tile = VSM.unsafeRead vram (area + tile + 0x2000)

{-# INLINE readTileData #-}
readTileData :: VRAM -> Int -> IO (Word8, Word8)
readTileData VRAM {..} tile = do
  b0 <- VSM.unsafeRead vram tile
  b1 <- VSM.unsafeRead vram (tile + 1)
  pure (b0, b1)

{-# INLINE readBankedTileData #-}
readBankedTileData :: VRAM -> Int -> IO (Word8, Word8)
readBankedTileData VRAM {..} tile = do
  b0 <- VSM.unsafeRead vram (tile + 0x2000)
  b1 <- VSM.unsafeRead vram (tile + 1 + 0x2000)
  pure (b0, b1)

{-# INLINE readOAM #-}
readOAM :: VRAM -> Word16 -> IO Word8
readOAM VRAM {..} addr = VSM.unsafeRead oam (fromIntegral (addr - 0xFE00))

{-# INLINE writeOAM #-}
writeOAM :: VRAM -> Word16 -> Word8 -> IO ()
writeOAM VRAM {..} addr = VSM.unsafeWrite oam (fromIntegral (addr - 0xFE00))

{-# INLINE readVRAM #-}
readVRAM :: VRAM -> Word16 -> IO Word8
readVRAM VRAM {..} addr = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure 0xFF
    else do
      bankOffset <- readUnboxedRef vramBank
      VSM.unsafeRead vram (fromIntegral (addr - 0x8000) + bankOffset)

{-# INLINE writeVRAM #-}
writeVRAM :: VRAM -> Word16 -> Word8 -> IO ()
writeVRAM VRAM {..} addr value = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure ()
    else do
      bankOffset <- readUnboxedRef vramBank
      VSM.unsafeWrite vram (fromIntegral (addr - 0x8000) + bankOffset) value

-- Copy a slice of memory into OAM.  The slice MUST have length 160.
copyToOAM :: VRAM -> VSM.IOVector Word8 -> IO ()
copyToOAM VRAM {..} = VSM.unsafeMove oam

-- DMA from VRAM to OAM does not respect the VRAM bank according to the docs.
copyVRAMToOAM :: VRAM -> Word16 -> IO ()
copyVRAMToOAM VRAM {..} from =
  VSM.unsafeMove oam (VSM.unsafeSlice (fromIntegral from - 0x8000) 160 vram)

encodeColor :: ColorFunction -> Word16 -> Word32
encodeColor correction color =
  let b            = (color .>>. 10) .&. 0x1F
      g            = (color .>>. 5) .&. 0x1F
      r            = color .&. 0x1F
      (r', g', b') = correction (r, g, b)
  in  (b' .<<. 16) .|. (g' .<<. 8) .|. r'

vgaColors :: ColorFunction
vgaColors (r, g, b) = (fromIntegral (8 * r), fromIntegral (8 * g), fromIntegral (8 * b))

defaultColors :: ColorFunction
defaultColors (r, g, b) =
  let r' = 960 `min` (r * 26 + g * 4 + b * 2)
      g' = 960 `min` (g * 24 + b * 8)
      b' = 960 `min` (r * 6 + g * 4 + b * 22)
  in  (fromIntegral (r' `div` 4), fromIntegral (g' `div` 4), fromIntegral (b' `div` 4))
