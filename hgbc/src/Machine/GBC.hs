{-# LANGUAGE RecordWildCards #-}
module Machine.GBC
  ( initEmulatorState
  , reset
  , parseROM
  , newGraphicsSync
  , audioBuffer
  , getEmulatorClock
  , writeFgRGBPalette
  , writeBgRGBPalette
  , step
  , keyUp
  , keyDown
  , ROMPaths(..)
  , ROM(..)
  , Key(..)
  , Header(..)
  , GraphicsSync(..)
  , EmulatorState(..)
  )
where

import           Control.Monad.Reader
import           Data.Word
import           Machine.GBC.Audio
import           Machine.GBC.CPU
import           Machine.GBC.Emulator
import           Machine.GBC.Graphics
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Keypad
import           Machine.GBC.Primitive
import           Machine.GBC.ROM

-- | Notify that a key is pressed down.
keyDown :: Key -> ReaderT EmulatorState IO ()
keyDown key = do
  EmulatorState {..} <- ask
  lift $ keypadPress keypadState key

-- | Notify that a key is released.
keyUp :: Key -> ReaderT EmulatorState IO ()
keyUp key = do
  EmulatorState {..} <- ask
  lift $ keypadRelease keypadState key

-- | Get the audio output buffer.
audioBuffer :: EmulatorState -> RingBuffer Word16
audioBuffer = audioOut . audioState

-- | Set a foreground palette.
writeFgRGBPalette
  :: EmulatorState
  -> Int     -- ^ The foreground palette number (0 to 7) to write.
  -> Word32  -- ^ Color 0
  -> Word32  -- ^ Color 1
  -> Word32  -- ^ Color 2
  -> Word32  -- ^ Color 3
  -> IO ()
writeFgRGBPalette EmulatorState {..} = writeRGBPalette vram True

-- | Set a background palette.
writeBgRGBPalette
  :: EmulatorState
  -> Int     -- ^ The background palette number (0 to 7) to write.
  -> Word32  -- ^ Color 0
  -> Word32  -- ^ Color 1
  -> Word32  -- ^ Color 2
  -> Word32  -- ^ Color 3
  -> IO ()
writeBgRGBPalette EmulatorState {..} = writeRGBPalette vram False
