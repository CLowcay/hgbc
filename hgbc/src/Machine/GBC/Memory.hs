{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Memory
  ( Memory
  , HasMemory(..)
  , resetAndBoot
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
import           Data.Functor
import           Data.IORef
import           Data.Maybe
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
    mbc            :: !MBC
  , mode0          :: !EmulatorMode
  , modeRef        :: !(IORef EmulatorMode)
  , header         :: !Header
  , rom0           :: !(IORef (VS.Vector Word8))  -- The first 8kb of ROM. Can be switched between cartridge ROM and boot ROM.
  , rom            :: !(VS.Vector Word8)          -- All of cartrige ROM.
  , bootROM        :: !(Maybe (VS.Vector Word8))
  , bootROMLockout :: !(IORef Bool)
  , vram           :: !VRAM
  , memRam         :: !(VSM.IOVector Word8)
  , ramBankOffset  :: !(UnboxedRef Int)
  , ports          :: !(V.Vector (Port Word8))
  , portIE         :: !(Port Word8)
  , portSVBK       :: !(Port Word8)
  , portBLCK       :: !(Port Word8)
  , portR4C        :: !(Port Word8)
  , portR6C        :: !(Port Word8)
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

-- | Reset the memory system. If there is no boot ROM, then also run the
-- supplied boot code.
resetAndBoot :: HasMemory env => ReaderT env IO () -> ReaderT env IO ()
resetAndBoot pseudoBootROM = do
  Memory {..} <- asks forMemory
  liftIO $ do
    writeIORef modeRef        mode0
    writeIORef bootROMLockout False
    writePort portSVBK 0
    directWritePort portBLCK 0
    directWritePort portR4C  0
    writePort portR6C 0
  case bootROM of
    Nothing      -> pseudoBootROM
    Just content -> liftIO $ writeIORef rom0 content

-- | The initial memory state.
initMemoryForROM
  :: Maybe (VS.Vector Word8)
  -> ROM
  -> VRAM
  -> [(Word16, Port Word8)]
  -> Port Word8
  -> IORef EmulatorMode
  -> IO Memory
initMemoryForROM boot romInfo vram ports portIE modeRef = do
  mbc <- getMBC romInfo
  initMemory boot
             (VS.fromList (B.unpack (romContent romInfo)))
             (romHeader romInfo)
             mbc
             vram
             ports
             portIE
             modeRef

initMemory
  :: Maybe (VS.Vector Word8)
  -> VS.Vector Word8
  -> Header
  -> MBC
  -> VRAM
  -> [(Word16, Port Word8)]
  -> Port Word8
  -> IORef EmulatorMode
  -> IO Memory
initMemory boot rom header mbc vram rawPorts portIE modeRef = do
  mode0 <- readIORef modeRef
  let bootROM = boot <&> \content ->
        let boot1 = VS.take 0x100 content
            boot2 = VS.drop 0x200 content
        in  if VS.length content <= 0x200
              then boot1 <> VS.drop (VS.length boot1) rom
              else boot1 <> VS.slice 0x100 0x100 rom <> boot2 <> VS.drop (VS.length content) rom

  memRam        <- VSM.new 0x10000
  memHigh       <- VSM.new 0x80
  rom0          <- newIORef $ fromMaybe rom bootROM

  emptyPort     <- newPort 0xFF 0x00 neverUpdate
  ramBankOffset <- newUnboxedRef 0

  -- SVBK: RAM bank.
  portSVBK      <- cgbOnlyPort modeRef 0xF8 0x07 $ \_ v' -> v' <$ do
    let bank = fromIntegral (v' .&. 7)
    writeUnboxedRef ramBankOffset (0 `max` ((bank - 1) * 0x1000))

  -- R4C: An undocumented register that appears to control DMG compatibility in
  -- the LCD.
  bootROMLockout <- newIORef False
  portR4C        <- newPortWithReadAction
    0x00
    0xFF
    (\a -> do
      lockout <- readIORef bootROMLockout
      pure (if lockout then 0xFF else a)
    )
    alwaysUpdate

  -- BLCK: BIOS lockout.
  portBLCK <- newPort 0xFE 0x01 $ \oldValue newValue -> do
    when (isFlagSet 1 newValue) $ do
      lcdMode <- directReadPort portR4C
      when (lcdMode == 4) (writeIORef modeRef DMG)
      writeIORef rom0           rom
      writeIORef bootROMLockout True
    pure (newValue .|. oldValue)

  -- Undocumented registers
  portR6C <- cgbOnlyPort modeRef 0xFE 0x01 alwaysUpdate
  r72     <- newPort 0x00 0xFF alwaysUpdate
  r73     <- newPort 0x00 0xFF alwaysUpdate
  r74     <- cgbOnlyPort modeRef 0x00 0xFF alwaysUpdate
  r75     <- newPort 0x8F 0x70 alwaysUpdate

  let ports = V.accum
        (flip const)
        (V.replicate 128 emptyPort)
        (   first portOffset
        <$> ( (BLCK, portBLCK)
            : (SVBK, portSVBK)
            : (R4C , portR4C)
            : (R6C , portR6C)
            : (R72 , r72)
            : (R73 , r73)
            : (R74 , r74)
            : (R75 , r75)
            : rawPorts
            )
        )

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
    0 -> do
      content <- readIORef rom0
      copyToOAM vram . VSM.unsafeSlice (fromIntegral source) oamSize =<< VS.unsafeThaw content
    1 -> copyToOAM vram . VSM.unsafeSlice (fromIntegral source) oamSize =<< VS.unsafeThaw rom
    2 -> do
      bank <- bankOffset mbc
      copyToOAM vram . VSM.unsafeSlice (bank + offset 0x4000) oamSize =<< VS.unsafeThaw rom
    3 -> do
      bank <- bankOffset mbc
      copyToOAM vram . VSM.unsafeSlice (bank + offset 0x4000) oamSize =<< VS.unsafeThaw rom
    4 -> copyVRAMToOAM vram source
    5 -> copyToOAM vram =<< sliceRAM mbc (source - 0xA000) oamSize
    6
      | source < 0xD000 -> copyToOAM vram (VSM.unsafeSlice (offset 0xC000) oamSize memRam)
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
    0 -> do
      content <- readIORef rom0
      pure (content `VS.unsafeIndex` fromIntegral addr)
    1 -> pure (rom `VS.unsafeIndex` fromIntegral addr)
    2 -> do
      bank <- bankOffset mbc
      pure (rom `VS.unsafeIndex` (bank + offset 0x4000))
    3 -> do
      bank <- bankOffset mbc
      pure (rom `VS.unsafeIndex` (bank + offset 0x4000))
    4 -> readVRAM vram addr
    5 -> readRAM mbc (addr - 0xA000)
    6
      | addr < 0xD000 -> VSM.unsafeRead memRam (offset 0xC000)
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
    5 -> writeRAM mbc (addr - 0xA000) value
    6
      | addr < 0xD000 -> VSM.unsafeWrite memRam (offset 0xC000) value
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
