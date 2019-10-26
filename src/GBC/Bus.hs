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
import           Data.Bits

data BusState = BusState {
    cpu :: !CPUState
  , memory :: !Memory
  , graphics :: !(IORef GraphicsState)
  , graphicsSync :: !GraphicsSync
  , keypadState :: !(IORef Word8)
  , lastEventPollAt :: !(IORef Word32)
  , timerState :: !TimerState
  , breakFlag :: !(IORef Bool)
}

class HasBusState env where
  forBusState :: env -> BusState

instance HasBusState BusState where
  {-# INLINE forBusState #-}
  forBusState = id

instance HasBusState env => HasMemory env where
  {-# INLINE forMemory #-}
  forMemory = memory . forBusState

instance HasBusState env => HasCPUState env where
  {-# INLINE forCPUState #-}
  forCPUState = cpu . forBusState

instance HasBusState env => HasGraphicsState env where
  {-# INLINE forGraphicsState #-}
  forGraphicsState = graphics . forBusState
  forGraphicsSync = graphicsSync . forBusState

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
initBusState :: CPUState -> Memory -> GraphicsSync -> IO BusState
initBusState cpuState mem sync =
  BusState cpuState mem
    <$> newIORef initGraphics
    <*> pure sync
    <*> initKeypadState
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef False

-- | Check if the debug break flag is set.
{-# INLINABLE isBreakFlagSet #-}
isBreakFlagSet :: UsesBus env m => ReaderT env m Bool
isBreakFlagSet = liftIO . readIORef . breakFlag =<< asks forBusState

-- | Reset the debug break flag.
{-# INLINABLE clearBreakFlag #-}
clearBreakFlag :: UsesBus env m => ReaderT env m ()
clearBreakFlag = liftIO . flip writeIORef False . breakFlag =<< asks forBusState

-- | Close all windows.
{-# INLINABLE killWindows #-}
killWindows :: UsesBus env m => ReaderT env m ()
killWindows = do
  sync <- graphicsSync <$> asks forBusState
  liftIO (putMVar (currentLine sync) 255)

pattern ReleasedBreakKey :: SDL.EventPayload
pattern ReleasedBreakKey <- SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ SDL.KeycodePause _))

-- | Poll and dispatch events.
{-# INLINE handleEvents #-}
handleEvents :: UsesBus env m => ReaderT env m ()
handleEvents = do
  events <- SDL.pollEvents
  keypadHandleUserEvents events
  for_ (eventPayload <$> events) $ \case
    (SDL.WindowClosedEvent _) -> killWindows
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
busStep :: UsesBus env m => ReaderT env m BusEvent
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

  graphicsStep busEvent
  pure busEvent
