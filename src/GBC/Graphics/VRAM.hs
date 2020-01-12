{-# LANGUAGE RecordWildCards #-}
module GBC.Graphics.VRAM
  ( VRAM
  , initVRAM
  , setVRAMAccessible
  , setVRAMBank
  , writePalette
  , readPalette
  , readRGBPalette
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

import           Common
import           Data.Bits
import           Data.IORef
import           Data.Word
import           GBC.Mode
import           GBC.Primitive.UnboxedRef
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Storable.Mutable  as VSM

data VRAM = VRAM {
    vram           :: !(VUM.IOVector Word8)
  , oam            :: !(VUM.IOVector Word8)
  , rawPalettes    :: !(VSM.IOVector Word16)
  , rgbPalettes    :: !(VUM.IOVector Word32)
  , mode           :: !EmulatorMode
  , vramAccessible :: !(IORef Bool)
  , vramBank       :: !(UnboxedRef Int)
}

totalPaletteEntries :: Int
totalPaletteEntries = 8 * 4 * 2 -- 2 sets of 8 palettes with 4 colors each.

initVRAM :: EmulatorMode -> IO VRAM
initVRAM mode = do
  let size = case mode of
        DMG -> 0x2000
        CGB -> 0x4000
  vram           <- VUM.new size
  oam            <- VUM.new 160
  vramAccessible <- newIORef True
  vramBank       <- newUnboxedRef 0
  rawPalettes    <- VSM.replicate totalPaletteEntries 0x7FFF
  rgbPalettes    <- VUM.replicate totalPaletteEntries 0xFFFFFFFF
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
{-# INLINE writePalette #-}
writePalette :: VRAM -> Bool -> Word8 -> Word8 -> IO ()
writePalette VRAM {..} fg cps cpd = do
  VSM.write (VSM.unsafeCast rawPalettes) (paletteByte fg cps) cpd
  let i = paletteIndex fg (cps .>>. 1)
  raw <- VSM.read rawPalettes i
  VUM.write rgbPalettes i (encodeColor raw)

-- | Read a palette given the value of the cps register.
{-# INLINE readPalette #-}
readPalette :: VRAM -> Bool -> Word8 -> IO Word8
readPalette VRAM {..} fg cps = VSM.read (VSM.unsafeCast rawPalettes) (paletteByte fg cps)

-- | Read a decoded RGB palette with the given 5-bit address
{-# INLINE readRGBPalette #-}
readRGBPalette :: VRAM -> Bool -> Word8 -> IO Word32
readRGBPalette VRAM {..} fg addr = VUM.read rgbPalettes (paletteIndex fg addr)

encodeColor :: Word16 -> Word32
encodeColor color =
  let b            = (color .>>. 10) .&. 0x1F
      g            = (color .>>. 5) .&. 0x1F
      r            = color .&. 0x1F
      (r', g', b') = cgbColors (fromIntegral r, fromIntegral g, fromIntegral b)
  in  (fromIntegral b' .<<. 16) .|. (fromIntegral g' .<<. 8) .|. fromIntegral r'

type ColorCorrection = (Int, Int, Int) -> (Word8, Word8, Word8)

cgbColors :: ColorCorrection
cgbColors (r, g, b) =
  let r' = 960 `min` (r * 26 + g * 4 + b * 2)
      g' = 960 `min` (g * 24 + b * 8)
      b' = 960 `min` (r * 6 + g * 4 + b * 22)
  in  (fromIntegral (r' `div` 4), fromIntegral (g' `div` 4), fromIntegral (b' `div` 4))

{-# INLINE readSpritePosition #-}
readSpritePosition :: VRAM -> Int -> IO (Word8, Word8)
readSpritePosition VRAM {..} i = do
  y <- VUM.read oam i
  x <- VUM.read oam (i + 1)
  pure (y, x)

{-# INLINE readSpriteAttributes #-}
readSpriteAttributes :: VRAM -> Int -> IO (Word8, Word8)
readSpriteAttributes VRAM {..} i = do
  c <- VUM.read oam (i + 2)
  a <- VUM.read oam (i + 3)
  pure (c, a)

{-# INLINE readTile #-}
readTile :: VRAM -> Int -> Int -> IO Word8
readTile VRAM {..} area tile = VUM.read vram (area + tile)

-- WARNING: DO NOT CALL unless running in DMG mode.
{-# INLINE readTileAttrs #-}
readTileAttrs :: VRAM -> Int -> Int -> IO Word8
readTileAttrs VRAM {..} area tile = VUM.read vram (area + tile + 0x2000)

{-# INLINE readTileData #-}
readTileData :: VRAM -> Int -> IO (Word8, Word8)
readTileData VRAM {..} tile = do
  b0 <- VUM.read vram tile
  b1 <- VUM.read vram (tile + 1)
  pure (b0, b1)

{-# INLINE readBankedTileData #-}
readBankedTileData :: VRAM -> Int -> IO (Word8, Word8)
readBankedTileData VRAM {..} tile = do
  b0 <- VUM.read vram (tile + 0x2000)
  b1 <- VUM.read vram (tile + 1 + 0x2000)
  pure (b0, b1)

{-# INLINE readOAM #-}
readOAM :: VRAM -> Word16 -> IO Word8
readOAM VRAM {..} addr = VUM.read oam (fromIntegral (addr - 0xFE00))

{-# INLINE writeOAM #-}
writeOAM :: VRAM -> Word16 -> Word8 -> IO ()
writeOAM VRAM {..} addr = VUM.write oam (fromIntegral (addr - 0xFE00))

{-# INLINE readVRAM #-}
readVRAM :: VRAM -> Word16 -> IO Word8
readVRAM VRAM {..} addr = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure 0xFF
    else do
      bankOffset <- readUnboxedRef vramBank
      VUM.read vram (fromIntegral (addr - 0x8000) + bankOffset)

{-# INLINE writeVRAM #-}
writeVRAM :: VRAM -> Word16 -> Word8 -> IO ()
writeVRAM VRAM {..} addr value = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure ()
    else do
      bankOffset <- readUnboxedRef vramBank
      VUM.write vram (fromIntegral (addr - 0x8000) + bankOffset) value

-- Copy a slice of memory into OAM.  The slice MUST have length 160.
{-# INLINE copyToOAM #-}
copyToOAM :: VRAM -> VUM.IOVector Word8 -> IO ()
copyToOAM VRAM {..} = VUM.move oam

-- DMA from VRAM to OAM does not respect the VRAM bank according to the docs.
{-# INLINE copyVRAMToOAM #-}
copyVRAMToOAM :: VRAM -> Word16 -> IO ()
copyVRAMToOAM VRAM {..} from = VUM.move oam (VUM.slice (fromIntegral from - 0x8000) 160 vram)
