{-# LANGUAGE RecordWildCards #-}
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
import           GBC.Timer
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
  , lastEventPollAt :: !(IORef Word32)
  , timerState :: !TimerState
}

class HasBusState env where
  forBusState :: env -> BusState

instance HasBusState env => HasMemory env where
  {-# INLINE forMemory #-}
  forMemory = memory . forBusState

instance HasBusState env => HasCPUState env where
  {-# INLINE forCPUState #-}
  forCPUState = cpu . forBusState

instance HasBusState env => HasGraphicsState env where
  {-# INLINE forGraphicsState #-}
  forGraphicsState = graphics . forBusState

instance HasBusState env => HasKeypadState env where
  {-# INLINE forKeypadState #-}
  forKeypadState = keypadState . forBusState

instance HasBusState env => HasTimerState env where
  {-# INLINE forTimerState #-}
  forTimerState = timerState . forBusState

type UsesBus env m = (HasBusState env, UsesCPU env m, UsesKeypad env m)

pollDelay :: Word32
pollDelay = 10

initBusState :: CPUState -> Memory -> IO BusState
initBusState cpuState mem =
  BusState cpuState mem
    <$> newIORef initGraphics
    <*> H.new
    <*> initKeypadState
    <*> newIORef 0
    <*> newIORef 0

registerWindow :: UsesBus env m => Window -> MVar (Maybe Update) -> ReaderT env m ()
registerWindow window queue = do
  windows <- graphicsOutput <$> asks forBusState
  liftIO $ H.insert windows window queue

{-# INLINABLE killWindow #-}
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

{-# INLINE handleEvents #-}
handleEvents :: UsesBus env m => ReaderT env m ()
handleEvents = do
  events <- pollEvents
  keypadHandleUserEvents events
  for_ (eventPayload <$> events) $ \case
    (WindowClosedEvent d) -> killWindow (windowClosedEventWindow d)
    _                     -> pure ()

{-# INLINABLE busStep #-}
busStep :: UsesBus env m => ReaderT env m (BusEvent, Maybe Update)
busStep = do
  busEvent <- cpuStep
  keypadHandleBusEvent busEvent

  BusState {..} <- asks forBusState
  now           <- ticks
  lastPoll      <- liftIO $ readIORef lastEventPollAt
  when (now - lastPoll > pollDelay) $ do
    handleEvents
    liftIO $ writeIORef lastEventPollAt =<< ticks

  graphicsUpdate <- graphicsStep busEvent
  case graphicsUpdate of
    Nothing     -> pure ()
    Just update -> liftIO $ traverse_ ((`putMVar` Just update) . snd) =<< H.toList graphicsOutput
  pure (busEvent, graphicsUpdate)
