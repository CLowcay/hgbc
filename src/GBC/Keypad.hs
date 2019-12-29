{-# LANGUAGE RecordWildCards #-}

module GBC.Keypad
  ( KeypadState
  , initKeypadState
  , keypadHandleUserEvents
  , keypadPorts
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Function
import           Data.IORef
import           Data.Word
import           GBC.Interrupts
import           GBC.Primitive
import           GBC.Registers
import           SDL.Event
import           SDL.Input.Keyboard

-- | Keypad key flags.
keyA, keyB, keySELECT, keySTART, keyLEFT, keyRIGHT, keyUP, keyDOWN :: Word8
keyA = 0x01
keyB = 0x02
keySELECT = 0x04
keySTART = 0x08
keyRIGHT = 0x10
keyLEFT = 0x20
keyUP = 0x40
keyDOWN = 0x80

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

-- | Update the keypad state given an SDL event payload.
updateKeyboardState :: EventPayload -> Word8 -> Word8
updateKeyboardState (KeyboardEvent d) keypadState =
  let symbol = case keysymKeycode (keyboardEventKeysym d) of
        KeycodeZ         -> Just keyA
        KeycodeX         -> Just keyB
        KeycodeReturn    -> Just keySTART
        KeycodeBackspace -> Just keySELECT
        KeycodeRight     -> Just keyRIGHT
        KeycodeLeft      -> Just keyLEFT
        KeycodeUp        -> Just keyUP
        KeycodeDown      -> Just keyDOWN
        _                -> Nothing
  in  maybe keypadState updateBits symbol
 where
  updateBits x = case keyboardEventKeyMotion d of
    Pressed  -> keypadState .|. x
    Released -> keypadState .&. complement x
updateKeyboardState _ keypadState = keypadState

-- | Update the 'regKeypad' register.
{-# INLINE refreshKeypad #-}
refreshKeypad :: Word8 -> Port Word8 -> Word8 -> Word8 -> IO Word8
refreshKeypad keypad portIF _ p1 = do
  let p1' = case (p1 `testBit` 4, p1 `testBit` 5) of
        (True , False) -> complement keypad .&. 0x0F
        (False, True ) -> complement (keypad .>>. 4)
        (True , True ) -> 0xFF
        (False, False) -> 0
  when (0 /= 0x0F .&. p1 .&. complement p1') (raiseInterrupt portIF InterruptP1Low)
  pure p1'

-- | Update the keypad state from a list of SDL events.
{-# INLINE keypadHandleUserEvents #-}
keypadHandleUserEvents :: KeypadState -> [Event] -> IO ()
keypadHandleUserEvents KeypadState {..} events = do
  keypad0 <- readIORef keypadRef
  let keypad1 = foldl (&) keypad0 (updateKeyboardState . eventPayload <$> events)
  writeIORef keypadRef keypad1
  p1 <- readPort portP1
  when (keypad0 /= keypad1) $ void $ refreshKeypad keypad1 portIF p1 p1

keypadPorts :: KeypadState -> [(Word16, Port Word8)]
keypadPorts KeypadState {..} = [(P1, portP1)]
