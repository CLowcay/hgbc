{-# LANGUAGE RecordWildCards #-}
module GBC.Keypad
  ( KeypadState
  , Keypad
  , initKeypadState
  , processBusEvent
  , processEvents
  )
where

import           Data.Word
import           Data.Bits
import           Data.Function
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           SDL.Input.Keyboard.Codes
import           SDL.Input.Keyboard
import           GBC.Memory
import           GBC.CPU
import           SDL.Event

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

-- | The keypad register.
regKeypad :: Word16
regKeypad = 0xFF00

-- | The current state of the keypad.
newtype KeypadState = KeypadState {unKeypadState :: Word8} deriving (Eq, Ord, Show)

-- | Create the initial keypad state.
initKeypadState :: KeypadState
initKeypadState = KeypadState 0x0F

-- | The keypad monad.
type Keypad a = ReaderT Memory (StateT KeypadState IO) a

-- | Update the keypad state given an SDL event payload.
updateKeyboardState :: EventPayload -> Word8 -> Word8
updateKeyboardState (KeyboardEvent d) keypadState =
  let symbol = case keysymKeycode $ keyboardEventKeysym d of
        KeycodeZ         -> Just keyA
        KeycodeX         -> Just keyB
        KeycodeReturn    -> Just keySELECT
        KeycodeBackspace -> Just keySTART
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
refreshKeypad :: Keypad ()
refreshKeypad = do
  KeypadState keypad <- get
  p1                 <- readByte regKeypad
  let p1' = case (p1 `testBit` 4, p1 `testBit` 5) of
        (True , False) -> (p1 .&. 0xF0) .|. (complement keypad .&. 0x0F)
        (False, True ) -> (p1 .&. 0xF0) .|. (complement (keypad `shiftR` 4) .&. 0x0F)
        _              -> p1
  writeMem regKeypad p1'

-- | Update the keypad state from a list of SDL events.
processEvents :: [Event] -> Keypad ()
processEvents events = do
  keypad0 <- get
  let keypad1 = KeypadState
        $ foldl (&) (unKeypadState keypad0) (updateKeyboardState . eventPayload <$> events)
  when (keypad0 /= keypad1) refreshKeypad

-- | Watch for bus events that might require us to refresh the 'regKeypad' register.
processBusEvent :: BusEvent -> Keypad ()
processBusEvent BusEvent {..} = when (regKeypad `elem` writeAddress) refreshKeypad
