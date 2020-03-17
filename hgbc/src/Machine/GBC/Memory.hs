{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Memory
  ( Memory
  , HasMemory(..)
  , initMemory
  , initMemoryForROM
  , getROMHeader
  , getMbcRegisters
  , dmaToOAM
  , readByte
  , writeByte
  , writeWord
  , copy16
  , readChunk
  )
where

import           Control.Exception              ( throwIO )
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.Errors
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Mode
import           Machine.GBC.Primitive
import           Machine.GBC.Primitive.UnboxedRef
import           Machine.GBC.ROM
import           Machine.GBC.Registers
import           Machine.GBC.Util
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as VSM

-- | The gameboy memory.
data Memory = Memory {
    mode           :: !EmulatorMode
  , mbc            :: !MBC
  , header         :: !Header
  , rom            :: !(VS.Vector Word8)
  , vram           :: !VRAM
  , memRam         :: !(VSM.IOVector Word8)
  , ramBankOffset  :: !(UnboxedRef Int)
  , ports          :: !(V.Vector (Port Word8))
  , portIE         :: !(Port Word8)
  , memHigh        :: !(VSM.IOVector Word8)
  , checkRAMAccess :: !(IORef Bool)
}

class HasMemory env where
  forMemory :: env -> Memory

instance HasMemory Memory where
  {-# INLINE forMemory #-}
  forMemory = id

-- | Convert an address into an internal port number.
portOffset :: Word16 -> Int
portOffset = subtract 0xFF00 . fromIntegral

-- | The initial memory state.
initMemoryForROM :: ROM -> VRAM -> [(Word16, Port Word8)] -> Port Word8 -> EmulatorMode -> IO Memory
initMemoryForROM romInfo vram ports portIE mode = do
  mbc <- getMBC romInfo
  initMemory (VS.fromList (B.unpack (romContent romInfo)))
             (romHeader romInfo)
             mbc
             vram
             ports
             portIE
             mode

initMemory
  :: VS.Vector Word8
  -> Header
  -> MBC
  -> VRAM
  -> [(Word16, Port Word8)]
  -> Port Word8
  -> EmulatorMode
  -> IO Memory
initMemory rom header mbc vram rawPorts portIE mode = do
  memRam        <- VSM.new 0x10000
  memHigh       <- VSM.new 0x80
  emptyPort     <- newPort 0xFF 0x00 neverUpdate

  ramBankOffset <- newUnboxedRef 0
  svbk          <- newPort 0xF8 0x07 $ \_ newValue -> do
    let bank = fromIntegral (newValue .&. 7)
    writeUnboxedRef ramBankOffset (0 `max` ((bank - 1) * 0x1000))
    pure newValue

  let ports = V.accum (flip const)
                      (V.replicate 128 emptyPort)
                      (first portOffset <$> ((SVBK, svbk) : rawPorts))

  checkRAMAccess <- newIORef False
  pure Memory { .. }

-- | Get the ROM header.
getROMHeader :: Memory -> Header
getROMHeader Memory {..} = header

-- | Get the current state of the MBC registers.
getMbcRegisters :: HasMemory env => ReaderT env IO [RegisterInfo]
getMbcRegisters = do
  Memory {..} <- asks forMemory
  liftIO (mbcRegisters mbc)

-- | Total number of bytes of OAM memory.
oamSize :: Int
oamSize = 160

-- | Copy data to OAM memory via DMA.
dmaToOAM :: HasMemory env => Word16 -> ReaderT env IO ()
dmaToOAM source = do
  Memory {..} <- asks forMemory
  liftIO $ case source .>>. 13 of
    0 -> copyToOAM vram . VSM.unsafeSlice (fromIntegral source) oamSize =<< VS.unsafeThaw rom
    1 -> copyToOAM vram . VSM.unsafeSlice (fromIntegral source) oamSize =<< VS.unsafeThaw rom
    2 -> do
      bank <- bankOffset mbc
      copyToOAM vram . VSM.unsafeSlice (bank + offset 0x4000) oamSize =<< VS.unsafeThaw rom
    3 -> do
      bank <- bankOffset mbc
      copyToOAM vram . VSM.unsafeSlice (bank + offset 0x4000) oamSize =<< VS.unsafeThaw rom
    4 -> copyVRAMToOAM vram source
    5 -> do
      check <- liftIO (readIORef checkRAMAccess)
      copyToOAM vram =<< sliceRAM mbc check (source - 0xA000) oamSize
    6
      | source < 0xD000 || mode == DMG -> copyToOAM
        vram
        (VSM.unsafeSlice (offset 0xC000) oamSize memRam)
      | otherwise -> do
        bank <- readUnboxedRef ramBankOffset
        copyToOAM vram (VSM.unsafeSlice (bank + offset 0xC000) oamSize memRam)
    _ -> liftIO (throwIO (InvalidSourceForDMA source))
  where offset base = fromIntegral source - base

-- | Read a byte from memory.
readByte :: HasMemory env => Word16 -> ReaderT env IO Word8
readByte addr = do
  Memory {..} <- asks forMemory
  liftIO $ case addr .>>. 13 of
    0 -> pure (rom `VS.unsafeIndex` fromIntegral addr)
    1 -> pure (rom `VS.unsafeIndex` fromIntegral addr)
    2 -> do
      bank <- bankOffset mbc
      pure (rom `VS.unsafeIndex` (bank + offset 0x4000))
    3 -> do
      bank <- bankOffset mbc
      pure (rom `VS.unsafeIndex` (bank + offset 0x4000))
    4 -> readVRAM vram addr
    5 -> do
      check <- liftIO (readIORef checkRAMAccess)
      readRAM mbc check (addr - 0xA000)
    6
      | addr < 0xD000 || mode == DMG -> VSM.unsafeRead memRam (offset 0xC000)
      | otherwise -> do
        bank <- readUnboxedRef ramBankOffset
        VSM.unsafeRead memRam (bank + offset 0xC000)
    7
      | addr < 0xFE00 -> VSM.unsafeRead memRam (offset 0xE000)
      | addr < 0xFEA0 -> do
        value <- readOAM vram addr
        pure (fromIntegral value)
      | addr < 0xFF00 -> liftIO $ do
        check <- readIORef checkRAMAccess
        if check then throwIO (InvalidRead (addr + 0xE000)) else pure 0xFF
      | addr < 0xFF80 -> liftIO $ readPort (ports V.! offset 0xFF00)
      | addr == IE -> liftIO $ readPort portIE
      | otherwise -> VSM.unsafeRead memHigh (offset 0xFF80)
    x -> error ("Impossible coarse read address " ++ show x)
  where offset base = fromIntegral addr - base

-- | Write a word to memory.
writeWord :: HasMemory env => Word16 -> Word16 -> ReaderT env IO ()
writeWord addr value = do
  writeByte addr       (fromIntegral (value .&. 0xFF))
  writeByte (addr + 1) (fromIntegral (value .>>. 8))

-- | Write to memory.
writeByte :: HasMemory env => Word16 -> Word8 -> ReaderT env IO ()
writeByte addr value = do
  Memory {..} <- asks forMemory
  liftIO $ case addr .>>. 13 of
    0 -> writeROM mbc addr value
    1 -> writeROM mbc addr value
    2 -> writeROM mbc addr value
    3 -> writeROM mbc addr value
    4 -> writeVRAM vram addr value
    5 -> do
      check <- liftIO (readIORef checkRAMAccess)
      writeRAM mbc check (addr - 0xA000) value
    6
      | addr < 0xD000 || mode == DMG -> VSM.unsafeWrite memRam (offset 0xC000) value
      | otherwise -> do
        bank <- readUnboxedRef ramBankOffset
        VSM.unsafeWrite memRam (bank + offset 0xC000) value
    7
      | addr < 0xFE00 -> VSM.unsafeWrite memRam (offset 0xE000) value
      | addr < 0xFEA0 -> writeOAM vram addr value
      | addr < 0xFF00 -> do
        check <- readIORef checkRAMAccess
        if check then throwIO (InvalidWrite (addr + 0xE000)) else pure ()
      | addr < 0xFF80 -> writePort (ports V.! offset 0xFF00) value
      | addr == IE -> liftIO $ writePort portIE value
      | otherwise -> VSM.unsafeWrite memHigh (offset 0xFF80) value
    x -> error ("Impossible coarse write address " ++ show x)
  where offset base = fromIntegral addr - base

-- | Copy 16 bytes from a source address to a destination address.
copy16 :: HasMemory env => Word16 -> Word16 -> ReaderT env IO ()
copy16 source destination = do
  writeByte destination =<< readByte source
  writeByte (destination + 1) =<< readByte (source + 1)
  writeByte (destination + 2) =<< readByte (source + 2)
  writeByte (destination + 3) =<< readByte (source + 3)
  writeByte (destination + 4) =<< readByte (source + 4)
  writeByte (destination + 5) =<< readByte (source + 5)
  writeByte (destination + 6) =<< readByte (source + 6)
  writeByte (destination + 7) =<< readByte (source + 7)
  writeByte (destination + 8) =<< readByte (source + 8)
  writeByte (destination + 9) =<< readByte (source + 9)
  writeByte (destination + 10) =<< readByte (source + 10)
  writeByte (destination + 11) =<< readByte (source + 11)
  writeByte (destination + 12) =<< readByte (source + 12)
  writeByte (destination + 13) =<< readByte (source + 13)
  writeByte (destination + 14) =<< readByte (source + 14)
  writeByte (destination + 15) =<< readByte (source + 15)

-- | Read a chunk of memory. Useful for debugging.
readChunk :: HasMemory env => Word16 -> Int -> ReaderT env IO B.ByteString
readChunk base len = B.pack <$> traverse readByte ((base +) <$> [0 .. fromIntegral len - 1])
