{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
module GBC.Keypad
  ( HasKeypadState(..)
  , UsesKeypad
  , initKeypadState
  , keypadHandleUserEvents
  , keypadHandleBusEvent
  )
where

import           Control.Monad.Reader
import           Data.Bits
import           Data.Function
import           Data.IORef
import           Data.Word
import           GBC.CPU
import           GBC.Memory
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

-- | The keypad register.
regKeypad :: Word16
regKeypad = 0xFF00

-- | Create the initial keypad state.
initKeypadState :: IO (IORef Word8)
initKeypadState = newIORef 0x00

class HasKeypadState env where
  forKeypadState :: env -> IORef Word8

type UsesKeypad env m = (HasKeypadState env, UsesMemory env m)

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
refreshKeypad :: UsesKeypad env m => ReaderT env m ()
refreshKeypad = do
  keypad <- liftIO . readIORef =<< asks forKeypadState
  p1     <- readByte regKeypad
  let p1' = case (p1 `testBit` 4, p1 `testBit` 5) of
        (True , False) -> (p1 .&. 0xF0) .|. (complement keypad .&. 0x0F)
        (False, True ) -> (p1 .&. 0xF0) .|. (complement (keypad `shiftR` 4) .&. 0x0F)
        _              -> p1
  writeMem regKeypad p1'

-- | Update the keypad state from a list of SDL events.
keypadHandleUserEvents :: UsesKeypad env m => [Event] -> ReaderT env m ()
keypadHandleUserEvents events = do
  keypad  <- asks forKeypadState
  keypad0 <- liftIO $ readIORef keypad
  let keypad1 = foldl (&) keypad0 (updateKeyboardState . eventPayload <$> events)
  liftIO $ writeIORef keypad keypad1
  when (keypad0 /= keypad1) refreshKeypad

-- | Watch for bus events that might require us to refresh the 'regKeypad' register.
keypadHandleBusEvent :: UsesKeypad env m => BusEvent -> ReaderT env m ()
keypadHandleBusEvent BusEvent {..} = when (regKeypad `elem` writeAddress) refreshKeypad
