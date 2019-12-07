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
import           Data.IORef
import           Data.Word
import           Data.Bits
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GBC.Mode

data VRAM = VRAM {
    vram           :: !(Ptr Word8)
  , oam            :: !(Ptr Word8)
  , rawPalettes    :: !(Ptr Word16)
  , rgbPalettes    :: !(Ptr Word32)
  , mode           :: !EmulatorMode
  , vramAccessible :: !(IORef Bool)
  , vramBank       :: !(IORef Int)
}

initVRAM :: EmulatorMode -> IO VRAM
initVRAM mode = do
  let size = case mode of
        DMG -> 0x2000
        CGB -> 0x4000
  vram           <- mallocBytes size
  oam            <- mallocBytes 160
  vramAccessible <- newIORef True
  vramBank       <- newIORef 0
  rawPalettes    <- mallocArray (8 * 4 * 2)
  rgbPalettes    <- mallocArray (8 * 4 * 2)
  pure VRAM { .. }

{-# INLINE setVRAMAccessible #-}
setVRAMAccessible :: VRAM -> Bool -> IO ()
setVRAMAccessible VRAM {..} = writeIORef vramAccessible

{-# INLINE setVRAMBank #-}
setVRAMBank :: VRAM -> Int -> IO ()
setVRAMBank VRAM {..} = writeIORef vramBank

-- | Write a palette given the values of the cps and cpd registers.
{-# INLINE writePalette #-}
writePalette :: VRAM -> Bool -> Word8 -> Word8 -> IO ()
writePalette r@VRAM {..} fg cps cpd = do
  pokeByteOff rawPalettes ((if fg then 64 else 0) + fromIntegral cps .&. 0x3F) cpd
  updatePalette r fg cps

-- | Read a palette given the value of the cps register.
{-# INLINE readPalette #-}
readPalette :: VRAM -> Bool -> Word8 -> IO Word8
readPalette VRAM {..} fg cps =
  peekByteOff rawPalettes ((if fg then 64 else 0) + fromIntegral cps .&. 0x3F)

-- | Read a decoded RGB palette with the given 5-bit address
{-# INLINE readRGBPalette #-}
readRGBPalette :: VRAM -> Bool -> Word8 -> IO Word32
readRGBPalette VRAM {..} fg addr =
  peekElemOff rgbPalettes ((if fg then 32 else 0) + fromIntegral addr .&. 0x1F)

updatePalette :: VRAM -> Bool -> Word8 -> IO ()
updatePalette VRAM {..} fg cps = do
  let addr = (if fg then 32 else 0) + fromIntegral ((cps `unsafeShiftR` 1) .&. 0x1F)
  raw <- peekElemOff rawPalettes addr
  pokeElemOff rgbPalettes addr (encodeColor raw)

encodeColor :: Word16 -> Word32
encodeColor color =
  let b = 8 * fromIntegral ((color `unsafeShiftR` 10) .&. 0x1F)
      g = 8 * fromIntegral ((color `unsafeShiftR` 5) .&. 0x1F)
      r = 8 * fromIntegral (color .&. 0x1F)
  in  (b `unsafeShiftL` 16) .|. (g `unsafeShiftL` 8) .|. r

{-# INLINE readSpritePosition #-}
readSpritePosition :: VRAM -> Int -> IO (Word8, Word8)
readSpritePosition VRAM {..} i = do
  y <- peekElemOff oam i
  x <- peekElemOff oam (i + 1)
  pure (y, x)

{-# INLINE readSpriteAttributes #-}
readSpriteAttributes :: VRAM -> Int -> IO (Word8, Word8)
readSpriteAttributes VRAM {..} i = do
  c <- peekElemOff oam (i + 2)
  a <- peekElemOff oam (i + 3)
  pure (c, a)

{-# INLINE readTile #-}
readTile :: VRAM -> Int -> Int -> IO Word8
readTile VRAM {..} area tile = peekElemOff vram (area + tile)

-- WARNING: DO NOT CALL unless running in DMG mode.
{-# INLINE readTileAttrs #-}
readTileAttrs :: VRAM -> Int -> Int -> IO Word8
readTileAttrs VRAM {..} area tile = peekElemOff vram (area + tile + 0x2000)

{-# INLINE readTileData #-}
readTileData :: VRAM -> Int -> IO (Word8, Word8)
readTileData VRAM {..} tile = do
  b0 <- peekElemOff vram tile
  b1 <- peekElemOff vram (tile + 1)
  pure (b0, b1)

{-# INLINE readBankedTileData #-}
readBankedTileData :: VRAM -> Int -> IO (Word8, Word8)
readBankedTileData VRAM {..} tile = do
  b0 <- peekElemOff vram (tile + 0x2000)
  b1 <- peekElemOff vram (tile + 1 + 0x2000)
  pure (b0, b1)

{-# INLINE readOAM #-}
readOAM :: VRAM -> Word16 -> IO Word8
readOAM VRAM {..} addr = peekElemOff oam (fromIntegral (addr - 0xFE00))

{-# INLINE writeOAM #-}
writeOAM :: VRAM -> Word16 -> Word8 -> IO ()
writeOAM VRAM {..} addr = pokeElemOff oam (fromIntegral (addr - 0xFE00))

{-# INLINE readVRAM #-}
readVRAM :: VRAM -> Word16 -> IO Word8
readVRAM VRAM {..} addr = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure 0xFF
    else case mode of
      DMG -> peekElemOff vram (fromIntegral (addr - 0x8000))
      CGB -> do
        bankOffset <- readIORef vramBank
        peekElemOff vram (fromIntegral (addr - 0x8000) + bankOffset)

{-# INLINE writeVRAM #-}
writeVRAM :: VRAM -> Word16 -> Word8 -> IO ()
writeVRAM VRAM {..} addr value = do
  isAccessible <- readIORef vramAccessible
  if not isAccessible
    then pure ()
    else case mode of
      DMG -> pokeElemOff vram (fromIntegral (addr - 0x8000)) value
      CGB -> do
        bankOffset <- readIORef vramBank
        pokeElemOff vram (fromIntegral (addr - 0x8000) + bankOffset) value

{-# INLINE copyToOAM #-}
copyToOAM :: VRAM -> Ptr Word8 -> IO ()
copyToOAM VRAM {..} from = moveArray oam from 160

-- DMA from VRAM to OAM does not respect the VRAM bank according to the docs.
{-# INLINE copyVRAMToOAM #-}
copyVRAMToOAM :: VRAM -> Word16 -> IO ()
copyVRAMToOAM r@VRAM {..} from = copyToOAM r (vram `plusPtr` fromIntegral (from - 0x8000))
