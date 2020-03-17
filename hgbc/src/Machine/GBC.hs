{-# LANGUAGE RecordWildCards #-}
module Machine.GBC
  ( initEmulatorState
  , parseROM
  , newGraphicsSync
  , audioBuffer
  , getEmulatorClock
  , step
  , keyUp
  , keyDown
  , ROMPaths(..)
  , ROM(..)
  , Header(..)
  , GraphicsSync(..)
  )
where

import           Control.Monad.Reader
import           Data.Word
import           Machine.GBC.Audio
import           Machine.GBC.Emulator
import           Machine.GBC.Graphics
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
