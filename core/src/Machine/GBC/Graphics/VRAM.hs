{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Graphics.VRAM
  ( VRAM,
    init,
    setAccessible,
    setOAMAccessible,
    getBank,
    setBank,
    writePalette,
    readPalette,
    readRGBPalette,
    writeRGBPalette,
    readSpritePosition,
    readSpriteAttributes,
    readTile,
    readTileAttrs,
    readTileData,
    readBankedTileData,
    readOAM,
    writeOAM,
    writeOAMDirect,
    read,
    readBankOffset,
    write,
  )
where

import Control.Monad (when)
import Data.Bits (Bits ((.&.), (.|.)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word16, Word32, Word8)
import qualified Machine.GBC.Color as Color
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import Machine.GBC.Util ((.<<.), (.>>.))
import Prelude hiding (init, read)

data VRAM = VRAM
  { vram :: !(VSM.IOVector Word8),
    oam :: !(VSM.IOVector Word8),
    rawPalettes :: !(VSM.IOVector Word16),
    rgbPalettes :: !(VSM.IOVector Word32),
    oamAccessible :: !(IORef Bool),
    vramAccessible :: !(IORef Bool),
    vramBank :: !(UnboxedRef Int),
    colorFunction :: !Color.Correction
  }

totalPaletteEntries :: Int
totalPaletteEntries = 8 * 4 * 2 -- 2 sets of 8 palettes with 4 colors each.

init :: Color.Correction -> IO VRAM
init colorFunction = do
  vram <- VSM.new 0x4000
  oam <- VSM.new 160
  oamAccessible <- newIORef True
  vramAccessible <- newIORef True
  vramBank <- newUnboxedRef 0
  rawPalettes <- VSM.replicate totalPaletteEntries 0x7FFF
  rgbPalettes <- VSM.replicate totalPaletteEntries 0xFFFFFFFF
  pure VRAM {..}

{-# INLINE setAccessible #-}
setAccessible :: VRAM -> Bool -> IO ()
setAccessible VRAM {..} = writeIORef vramAccessible

{-# INLINE setOAMAccessible #-}
setOAMAccessible :: VRAM -> Bool -> IO ()
setOAMAccessible VRAM {..} = writeIORef oamAccessible

{-# INLINE getBank #-}
getBank :: VRAM -> IO Int
getBank VRAM {..} = readUnboxedRef vramBank

{-# INLINE setBank #-}
setBank :: VRAM -> Int -> IO ()
setBank VRAM {..} = writeUnboxedRef vramBank

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
writeRGBPalette ::
  -- | Video RAM.
  VRAM ->
  -- | True to write a foreground palette, otherwise write a background palette.
  Bool ->
  -- | The palette number (0 to 7) to write.
  Int ->
  (Word32, Word32, Word32, Word32) ->
  IO ()
writeRGBPalette VRAM {..} fg i (c0, c1, c2, c3) =
  let base = 4 * i + if fg then 32 else 0
   in do
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
       in (a .<<. 24) .|. (b .<<. 16) .|. (g .<<. 8) .|. r

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
readOAM VRAM {..} addr = do
  accessible <- readIORef oamAccessible
  if accessible then VSM.unsafeRead oam (fromIntegral (addr - 0xFE00)) else pure 0xFF

{-# INLINE writeOAM #-}
writeOAM :: VRAM -> Word16 -> Word8 -> IO ()
writeOAM VRAM {..} addr value = do
  accessible <- readIORef oamAccessible
  when accessible $ VSM.unsafeWrite oam (fromIntegral (addr - 0xFE00)) value

{-# INLINE writeOAMDirect #-}
writeOAMDirect :: VRAM -> Word16 -> Word8 -> IO ()
writeOAMDirect VRAM {..} addr = VSM.unsafeWrite oam (fromIntegral addr)

{-# INLINE read #-}
read :: VRAM -> Word16 -> IO Word8
read VRAM {..} addr = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure 0xFF
    else do
      bankOffset <- readUnboxedRef vramBank
      VSM.unsafeRead vram (fromIntegral (addr - 0x8000) + bankOffset)

readBankOffset :: VRAM -> Int -> Word16 -> IO Word8
readBankOffset VRAM {..} bankOffset addr = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure 0xFF
    else VSM.unsafeRead vram (fromIntegral (addr - 0x8000) + bankOffset)

{-# INLINE write #-}
write :: VRAM -> Word16 -> Word8 -> IO ()
write VRAM {..} addr value = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure ()
    else do
      bankOffset <- readUnboxedRef vramBank
      VSM.unsafeWrite vram (fromIntegral (addr - 0x8000) + bankOffset) value

encodeColor :: Color.Correction -> Word16 -> Word32
encodeColor correction color =
  let b = (color .>>. 10) .&. 0x1F
      g = (color .>>. 5) .&. 0x1F
      r = color .&. 0x1F
      (r', g', b') = correction (r, g, b)
   in (b' .<<. 16) .|. (g' .<<. 8) .|. r'
