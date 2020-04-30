{-# LANGUAGE RecordWildCards #-}
module Machine.GBC
  ( initEmulatorState
  , CPU.reset
  , parseROM
  , requiresSaveFiles
  , Graphics.newSync
  , audioBuffer
  , getEmulatorClock
  , writeFgRGBPalette
  , writeBgRGBPalette
  , step
  , keyUp
  , keyDown
  , CPU.getCallDepth
  , CPU.getBacktrace
  , ROMPaths(..)
  , ROM(..)
  , ColorCorrection(..)
  , Keypad.Key(..)
  , Header(..)
  , Graphics.Sync(..)
  , EmulatorMode(..)
  , EmulatorState(..)
  )
where

import           Control.Monad.Reader
import           Data.Word
import           Machine.GBC.Emulator
import           Machine.GBC.Graphics.VRAM
import           Machine.GBC.Mode
import           Machine.GBC.Primitive
import           Machine.GBC.ROM
import qualified Machine.GBC.Audio             as Audio
import qualified Machine.GBC.CPU               as CPU
import qualified Machine.GBC.Graphics          as Graphics
import qualified Machine.GBC.Keypad            as Keypad

-- | Notify that a key is pressed down.
keyDown :: Keypad.Key -> ReaderT EmulatorState IO ()
keyDown key = do
  EmulatorState {..} <- ask
  lift $ Keypad.press keypadState key

-- | Notify that a key is released.
keyUp :: Keypad.Key -> ReaderT EmulatorState IO ()
keyUp key = do
  EmulatorState {..} <- ask
  lift $ Keypad.release keypadState key

-- | Get the audio output buffer.
audioBuffer :: EmulatorState -> RingBuffer Word16
audioBuffer = Audio.audioOut . audioState

-- | Set a foreground palette.
writeFgRGBPalette
  :: EmulatorState
  -> Int     -- ^ The foreground palette number (0 to 7) to write.
  -> (Word32, Word32, Word32, Word32)
  -> IO ()
writeFgRGBPalette EmulatorState {..} = writeRGBPalette vram True

-- | Set a background palette.
writeBgRGBPalette
  :: EmulatorState
  -> Int     -- ^ The background palette number (0 to 7) to write.
  -> (Word32, Word32, Word32, Word32)
  -> IO ()
writeBgRGBPalette EmulatorState {..} = writeRGBPalette vram False
