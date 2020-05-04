{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Memory
  ( State
  , Has(..)
  , resetAndBoot
  , init
  , initForROM
  , hasBootROM
  , bootROMLength
  , getROMData
  , getBootROMData
  , getBank
  , getRamGate
  , readByteLong
  , getROMHeader
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
import           Prelude                 hiding ( init )
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as VSM

-- | The gameboy memory.
data State = State {
    mbc            :: !MBC
  , mode0          :: !EmulatorMode
  , modeRef        :: !(IORef EmulatorMode)
  , header         :: !Header
  , rom0           :: !(IORef (VS.Vector Word8))  -- The first 8kb of ROM. Can be switched between cartridge ROM and boot ROM.
  , rom            :: !(VS.Vector Word8)          -- All of cartrige ROM.
  , bootROMLength  :: Int
  , bootROM        :: !(Maybe (VS.Vector Word8))
  , bootROMLockout :: !(IORef Bool)
  , vram           :: !VRAM
  , memRam         :: !(VSM.IOVector Word8)
  , internalRamBankOffset  :: !(UnboxedRef Int)
  , ports          :: !(V.Vector (Port Word8))
  , portIE         :: !(Port Word8)
  , portSVBK       :: !(Port Word8)
  , portBLCK       :: !(Port Word8)
  , portR4C        :: !(Port Word8)
  , portR6C        :: !(Port Word8)
  , memHigh        :: !(VSM.IOVector Word8)
  , checkRAMAccess :: !(IORef Bool)
}
class Has env where
  forState :: env -> State

instance Has State where
  {-# INLINE forState #-}
  forState = id

-- | Convert an address into an internal port number.
portOffset :: Word16 -> Int
portOffset = subtract 0xFF00 . fromIntegral

-- | Reset the memory system. If there is no boot ROM, then also run the
-- supplied boot code.
{-# INLINABLE resetAndBoot #-}
resetAndBoot :: Has env => ReaderT env IO () -> ReaderT env IO ()
resetAndBoot pseudoBootROM = do
  State {..} <- asks forState
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
initForROM
  :: Maybe (VS.Vector Word8)
  -> ROM
  -> VRAM
  -> [(Word16, Port Word8)]
  -> Port Word8
  -> IORef EmulatorMode
  -> IO State
initForROM boot romInfo vram ports portIE modeRef = do
  mbc <- getMBC romInfo
  init boot
       (VS.fromList (B.unpack (romContent romInfo)))
       (romHeader romInfo)
       mbc
       vram
       ports
       portIE
       modeRef

init
  :: Maybe (VS.Vector Word8)
  -> VS.Vector Word8
  -> Header
  -> MBC
  -> VRAM
  -> [(Word16, Port Word8)]
  -> Port Word8
  -> IORef EmulatorMode
  -> IO State
init boot rom header mbc vram rawPorts portIE modeRef = do
  mode0 <- readIORef modeRef
  let bootROMLength = maybe 0 VS.length boot
  let bootROM = boot <&> \content ->
        let boot1 = VS.take 0x100 content
            boot2 = VS.drop 0x200 content
        in  if VS.length content <= 0x200
              then boot1 <> VS.drop (VS.length boot1) rom
              else boot1 <> VS.slice 0x100 0x100 rom <> boot2 <> VS.drop (VS.length content) rom

  memRam                <- VSM.new 0x10000
  memHigh               <- VSM.new 0x80
  rom0                  <- newIORef $ fromMaybe rom bootROM

  emptyPort             <- newPort 0xFF 0x00 neverUpdate
  internalRamBankOffset <- newUnboxedRef 0

  -- SVBK: RAM bank.
  portSVBK              <- cgbOnlyPort modeRef 0x40 0xBF $ \_ v' -> v' <$ do
    let bank = fromIntegral (v' .&. 7)
    writeUnboxedRef internalRamBankOffset (0 `max` ((bank - 1) * 0x1000))

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
  pure State { .. }

-- | Get the ROM header.
getROMHeader :: State -> Header
getROMHeader State {..} = header

-- | Get all of the ROM bytes.
getROMData :: State -> VS.Vector Word8
getROMData State {..} = rom

-- | Get all of the boot ROM bytes.
getBootROMData :: State -> Maybe (VS.Vector Word8)
getBootROMData State {..} = VS.take bootROMLength <$> bootROM

-- | Check if a BOOT ROM is present.
hasBootROM :: State -> Bool
hasBootROM State {..} = isJust bootROM

-- | Get the current bank loaded at the specified address
{-# INLINABLE getBank #-}
getBank :: Has env => Word16 -> ReaderT env IO Word16
getBank address = do
  State {..} <- asks forState
  liftIO $ case address .>>. 13 of
    0 -> if (address < 0x100 || address >= 0x200) && address < fromIntegral bootROMLength
      then do
        lockout <- readIORef bootROMLockout
        pure (if lockout then 0 else 0xFFFF)
      else pure 0
    1 -> pure 0
    2 -> bankOffset mbc <&> \o -> fromIntegral (o .>>. 14)
    3 -> bankOffset mbc <&> \o -> fromIntegral (o .>>. 14)
    4 -> getVRAMBank vram <&> \o -> fromIntegral (o .>>. 13)
    5 -> ramBankOffset mbc <&> \o -> fromIntegral (o .>>. 13)
    6 -> if address < 0xD000
      then pure 0
      else readUnboxedRef internalRamBankOffset <&> \o -> fromIntegral (o .>>. 12)
    7 -> if address >= 0xF000 && address < 0xFE00
      then readUnboxedRef internalRamBankOffset <&> \o -> fromIntegral (o .>>. 12)
      else pure 0
    x -> error ("Impossible coarse address " ++ show x)

-- | Get the status of the cartridge RAM gate (True for enabled, False for
-- disabled).
getRamGate :: Has env => ReaderT env IO Bool
getRamGate = liftIO . ramGate . mbc =<< asks forState

-- | Read a byte from a specific memory bank.
readByteLong :: Has env => Word16 -> Word16 -> ReaderT env IO Word8
readByteLong bank addr = do
  State {..} <- asks forState
  liftIO $ case addr .>>. 13 of
    0 -> pure
      (if bank == 0xFFFF
        then maybe 0xFF (VS.! fromIntegral addr) bootROM
        else rom VS.! fromIntegral addr
      )
    1 -> pure (rom VS.! fromIntegral addr)
    2 -> pure (rom VS.! offsetWithBank 14 0x4000)
    3 -> pure (rom VS.! offsetWithBank 14 0x4000)
    4 -> readVRAMBankOffset vram (fromIntegral bank .<<. 13) addr
    5 -> readRAMBankOffset mbc (fromIntegral bank .<<. 13) (addr - 0xA000)
    6 | addr < 0xD000 -> VSM.read memRam (offset 0xC000)
      | otherwise     -> VSM.read memRam (offsetWithBank 12 0xC000)
    7
      | addr < 0xF000 -> VSM.read memRam (offset 0xE000)
      | addr < 0xFE00 -> VSM.unsafeRead memRam (offsetWithBank 12 0xE000)
      | addr < 0xFEA0 -> do
        value <- readOAM vram addr
        pure (fromIntegral value)
      | addr < 0xFF00 -> pure 0xFF
      | addr < 0xFF80 -> liftIO $ readPort (ports V.! offset 0xFF00)
      | addr == IE -> liftIO $ readPort portIE
      | otherwise -> VSM.read memHigh (offset 0xFF80)
    x -> error ("Impossible coarse read address " ++ show x)
 where
  offset base = fromIntegral addr - base
  offsetWithBank s base = fromIntegral addr - base + (fromIntegral bank .<<. s)

-- | Read a byte from memory.
{-# INLINABLE readByte #-}
readByte :: Has env => Word16 -> ReaderT env IO Word8
readByte addr = do
  State {..} <- asks forState
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
        bank <- readUnboxedRef internalRamBankOffset
        VSM.unsafeRead memRam (bank + offset 0xC000)
    7
      | addr < 0xF000 -> VSM.unsafeRead memRam (offset 0xE000)
      | addr < 0xFE00 -> do
        bank <- readUnboxedRef internalRamBankOffset
        VSM.unsafeRead memRam (bank + offset 0xE000)
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
writeWord :: Has env => Word16 -> Word16 -> ReaderT env IO ()
writeWord addr value = do
  writeByte addr       (fromIntegral (value .&. 0xFF))
  writeByte (addr + 1) (fromIntegral (value .>>. 8))

-- | Write to memory.
writeByte :: Has env => Word16 -> Word8 -> ReaderT env IO ()
writeByte addr value = do
  State {..} <- asks forState
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
        bank <- readUnboxedRef internalRamBankOffset
        VSM.unsafeWrite memRam (bank + offset 0xC000) value
    7
      | addr < 0xF000 -> VSM.unsafeWrite memRam (offset 0xE000) value
      | addr < 0xFE00 -> do
        bank <- readUnboxedRef internalRamBankOffset
        VSM.unsafeWrite memRam (bank + offset 0xE000) value
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
copy16 :: Has env => Word16 -> Word16 -> ReaderT env IO ()
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
readChunk :: Has env => Word16 -> Int -> ReaderT env IO B.ByteString
readChunk base len = B.pack <$> traverse readByte ((base +) <$> [0 .. fromIntegral len - 1])
