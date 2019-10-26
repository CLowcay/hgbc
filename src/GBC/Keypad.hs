{-# LANGUAGE RecordWildCards #-}

module GBC.Keypad
  ( HasKeypad(..)
  , KeypadState
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
initKeypadState :: IO (IORef Word8)
initKeypadState = newIORef 0x00

type KeypadState = IORef Word8

class HasMemory env => HasKeypad env where
  forKeypadState :: env -> KeypadState

-- | Update the keypad state given an SDL event payload.
updateKeyboardState :: EventPayload -> Word8 -> Word8
updateKeyboardState (KeyboardEvent d) keypadState =
  let symbol = case keysymKeycode $ keyboardEventKeysym d of
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
refreshKeypad :: HasKeypad env => ReaderT env IO ()
refreshKeypad = do
  keypad <- liftIO . readIORef =<< asks forKeypadState
  p1     <- readByte P1
  let p1' = case (p1 `testBit` 4, p1 `testBit` 5) of
        (True , False) -> (p1 .&. 0xF0) .|. (complement keypad .&. 0x0F)
        (False, True ) -> (p1 .&. 0xF0) .|. (complement (keypad `unsafeShiftR` 4) .&. 0x0F)
        _              -> p1
  writeByte P1 p1'
  when (0 /= 0x0F .&. p1 .&. complement p1') $ raiseInterrupt 4

-- | Update the keypad state from a list of SDL events.
{-# INLINE keypadHandleUserEvents #-}
keypadHandleUserEvents :: HasKeypad env => [Event] -> ReaderT env IO ()
keypadHandleUserEvents events = do
  keypad  <- asks forKeypadState
  keypad0 <- liftIO $ readIORef keypad
  let keypad1 = foldl (&) keypad0 (updateKeyboardState . eventPayload <$> events)
  liftIO $ writeIORef keypad keypad1
  when (keypad0 /= keypad1) refreshKeypad

-- | Watch for bus events that might require us to refresh the 'regKeypad' register.
{-# INLINE keypadHandleBusEvent #-}
keypadHandleBusEvent :: HasKeypad env => BusEvent -> ReaderT env IO ()
keypadHandleBusEvent BusEvent {..} = when (P1 `elem` writeAddress) refreshKeypad
