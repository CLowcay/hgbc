{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GBC.Bus
  ( BusState(..)
  , HasBusState(..)
  , UsesBus
  , initBusState
  , isBreakFlagSet
  , clearBreakFlag
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
import           SDL                           as SDL
import           SDL.Orphans                    ( )
import qualified Data.HashTable.IO             as H
import           Data.Bits

data BusState = BusState {
    cpu :: !CPUState
  , memory :: !Memory
  , graphics :: !(IORef GraphicsState)
  , graphicsOutput :: !(H.BasicHashTable Window (MVar (Maybe Update)))
  , keypadState :: !(IORef Word8)
  , lastEventPollAt :: !(IORef Word32)
  , timerState :: !TimerState
  , breakFlag :: !(IORef Bool)
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

-- | Number of milliseconds to wait between polling for events.
pollDelay :: Word32
pollDelay = 10

-- | Create an initial bus state.
initBusState :: CPUState -> Memory -> IO BusState
initBusState cpuState mem =
  BusState cpuState mem
    <$> newIORef initGraphics
    <*> H.new
    <*> initKeypadState
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef False

-- | Check if the debug break flag is set.
isBreakFlagSet :: UsesBus env m => ReaderT env m Bool
isBreakFlagSet = liftIO . readIORef . breakFlag =<< asks forBusState

-- | Reset the debug break flag.
clearBreakFlag :: UsesBus env m => ReaderT env m ()
clearBreakFlag = liftIO . flip writeIORef False . breakFlag =<< asks forBusState

-- | Register a window to recieve updates from the graphics subsytem.
registerWindow :: UsesBus env m => Window -> MVar (Maybe Update) -> ReaderT env m ()
registerWindow window queue = do
  windows <- graphicsOutput <$> asks forBusState
  liftIO $ H.insert windows window queue

-- | Close a registred window.
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

pattern ReleasedBreakKey :: SDL.EventPayload
pattern ReleasedBreakKey <- SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ SDL.KeycodePause _))

-- | Poll and dispatch events.
{-# INLINE handleEvents #-}
handleEvents :: UsesBus env m => ReaderT env m ()
handleEvents = do
  events <- SDL.pollEvents
  keypadHandleUserEvents events
  for_ (eventPayload <$> events) $ \case
    (SDL.WindowClosedEvent d) -> killWindow (windowClosedEventWindow d)
    ReleasedBreakKey          -> liftIO . flip writeIORef True . breakFlag =<< asks forBusState
    _                         -> pure ()

pattern DMA :: Word16
pattern DMA = 0xFF46

-- | Perform a DMA transfer.
{-# INLINABLE doDMA #-}
doDMA :: UsesMemory env m => BusEvent -> ReaderT env m ()
doDMA busEvent = when (DMA `elem` writeAddress busEvent) $ do
  address <- fromIntegral <$> readByte DMA
  dmaToOAM (address `shiftL` 8)

-- | Execute a single step of the emulation.
{-# INLINABLE busStep #-}
busStep :: UsesBus env m => ReaderT env m (BusEvent, Maybe Update)
busStep = do
  busEvent <- cpuStep
  keypadHandleBusEvent busEvent
  doDMA busEvent

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
