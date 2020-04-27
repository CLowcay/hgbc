{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Keypad
  ( KeypadState
  , Key(..)
  , initKeypadState
  , keypadPress
  , keypadRelease
  , keypadPorts
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Machine.GBC.CPU.Interrupts
import           Machine.GBC.Primitive
import           Machine.GBC.Registers
import           Machine.GBC.Util

-- | Create the initial keypad state.
initKeypadState :: Port Word8 -> IO KeypadState
initKeypadState portIF = do
  keypadRef <- newIORef 0x00
  portP1    <- newPort 0x00 0x30 $ \old new -> do
    keypad <- readIORef keypadRef
    refreshKeypad keypad portIF old new
  pure KeypadState { .. }

data KeypadState = KeypadState {
    keypadRef :: !(IORef Word8)
  , portP1    :: !(Port Word8)
  , portIF    :: !(Port Word8)
}

data Key = KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyA
         | KeyB
         | KeySelect
         | KeyStart
         deriving (Eq, Ord, Show)

keyFlag :: Key -> Word8
keyFlag KeyUp     = 0x40
keyFlag KeyDown   = 0x80
keyFlag KeyLeft   = 0x20
keyFlag KeyRight  = 0x10
keyFlag KeyA      = 0x01
keyFlag KeyB      = 0x02
keyFlag KeySelect = 0x04
keyFlag KeyStart  = 0x08

-- | Update the 'regKeypad' register.
{-# INLINE refreshKeypad #-}
refreshKeypad :: Word8 -> Port Word8 -> Word8 -> Word8 -> IO Word8
refreshKeypad keypad portIF _ p1 = do
  let keypad' = complement keypad
      p1'     = case (p1 `testBit` 4, p1 `testBit` 5) of
        (True , False) -> keypad' .&. 0x0F
        (False, True ) -> keypad' .>>. 4
        (True , True ) -> 0xFF
        (False, False) -> (keypad' .&. 0x0F) .|. (keypad' .>>. 4)
  when (0x0F .&. p1 .&. complement p1' /= 0) (raiseInterrupt portIF InterruptP1Low)
  pure (0xC0 .|. p1')

keypadPress :: KeypadState -> Key -> IO ()
keypadPress state@KeypadState {..} key = do
  keypad0 <- readIORef keypadRef
  let keypad1 = keypad0 .|. keyFlag key
  when (keypad0 /= keypad1) $ updateKeypadState state keypad1

keypadRelease :: KeypadState -> Key -> IO ()
keypadRelease state@KeypadState {..} key = do
  keypad0 <- readIORef keypadRef
  let keypad1 = keypad0 .&. complement (keyFlag key)
  when (keypad0 /= keypad1) $ updateKeypadState state keypad1

updateKeypadState :: KeypadState -> Word8 -> IO ()
updateKeypadState KeypadState {..} keypad = do
  atomicWriteIORef keypadRef $! keypad
  p1 <- readPort portP1
  void $ refreshKeypad keypad portIF p1 p1

keypadPorts :: KeypadState -> [(Word16, Port Word8)]
keypadPorts KeypadState {..} = [(P1, portP1)]
