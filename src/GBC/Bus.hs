{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GBC.Bus
  ( BusState(..)
  , HasBusState(..)
  , UsesBus
  , initBusState
  , registerWindow
  , busStep
  , handleEvents
  )
where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           GBC.CPU
import           GBC.Graphics
import           GBC.Keypad
import           GBC.Memory
import           SDL
import           SDL.Orphans                    ( )
import qualified Data.HashTable.IO             as H

data BusState = BusState {
    cpu :: !CPUState
  , memory :: !Memory
  , graphics :: !(IORef GraphicsState)
  , graphicsOutput :: !(H.BasicHashTable Window (MVar (Maybe Update)))
  , keypadState :: !(IORef Word8)
}

class HasBusState env where
  forBusState :: env -> BusState

instance HasBusState env => HasMemory env where
  forMemory = memory . forBusState

instance HasBusState env => HasCPUState env where
  forCPUState = cpu . forBusState

instance HasBusState env => HasGraphicsState env where
  forGraphicsState = graphics . forBusState

instance HasBusState env => HasKeypadState env where
  forKeypadState = keypadState . forBusState

type UsesBus env m = (HasBusState env, UsesCPU env m, UsesKeypad env m)

initBusState :: CPUState -> Memory -> IO BusState
initBusState cpuState mem =
  BusState cpuState mem <$> newIORef initGraphics <*> H.new <*> initKeypadState

registerWindow :: UsesBus env m => Window -> MVar (Maybe Update) -> ReaderT env m ()
registerWindow window queue = do
  windows <- graphicsOutput <$> asks forBusState
  liftIO $ H.insert windows window queue

killWindow :: UsesBus env m => Window -> ReaderT env m ()
killWindow window = do
  windows <- graphicsOutput <$> asks forBusState
  liftIO $ do
    mvar <- H.lookup windows window
    case mvar of
      Nothing  -> pure ()
      Just var -> do
        putMVar var Nothing
        H.delete windows window

handleEvents :: UsesBus env m => ReaderT env m ()
handleEvents = do
  events <- pollEvents
  keypadHandleUserEvents events
  for_ (eventPayload <$> events) $ \case
    (WindowClosedEvent d) -> killWindow (windowClosedEventWindow d)
    _                     -> pure ()

busStep :: UsesBus env m => ReaderT env m (BusEvent, Maybe Update)
busStep = do
  busEvent <- cpuStep
  keypadHandleBusEvent busEvent

  handleEvents

  graphicsUpdate <- graphicsStep busEvent
  case graphicsUpdate of
    Nothing     -> pure ()
    Just update -> do
      windows <- graphicsOutput <$> asks forBusState
      liftIO $ traverse_ ((`putMVar` Just update) . snd) =<< H.toList windows
  pure (busEvent, graphicsUpdate)
