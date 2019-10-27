{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Bus
  ( BusState(..)
  , HasBus(..)
  , initBusState
  , isBreakFlagSet
  , clearBreakFlag
  , busStep
  , handleEvents
  )
where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           GBC.CPU
import           GBC.Graphics
import           GBC.Keypad
import           GBC.Memory
import           GBC.Registers
import           GBC.Timer
import           SDL.Orphans                    ( )
import qualified SDL

data BusState = BusState {
    lastEventPollAt :: !(IORef Word32)
  , breakFlag :: !(IORef Bool)
}

class (HasMemory env, HasCPU env, HasGraphics env, HasKeypad env, HasTimer env) => HasBus env where
  forBusState :: env -> BusState

-- | Number of milliseconds to wait between polling for events.
pollDelay :: Word32
pollDelay = 10

-- | Create an initial bus state.
initBusState :: IO BusState
initBusState = BusState <$> newIORef 0 <*> newIORef False

-- | Check if the debug break flag is set.
{-# INLINABLE isBreakFlagSet #-}
isBreakFlagSet :: HasBus env => ReaderT env IO Bool
isBreakFlagSet = liftIO . readIORef . breakFlag =<< asks forBusState

-- | Reset the debug break flag.
{-# INLINABLE clearBreakFlag #-}
clearBreakFlag :: HasBus env => ReaderT env IO ()
clearBreakFlag = liftIO . flip writeIORef False . breakFlag =<< asks forBusState

-- | Close all windows.
{-# INLINABLE killWindows #-}
killWindows :: HasBus env => ReaderT env IO ()
killWindows = do
  sync <- asks forGraphicsSync
  liftIO (putMVar (currentLine sync) 255)

-- | Poll and dispatch events.
{-# INLINE handleEvents #-}
handleEvents :: HasBus env => ReaderT env IO ()
handleEvents = do
  events <- SDL.pollEvents
  keypadHandleUserEvents events
  for_ (SDL.eventPayload <$> events) $ \case
    (SDL.WindowClosedEvent _) -> killWindows
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ SDL.KeycodePause _)) ->
      liftIO . flip writeIORef True . breakFlag =<< asks forBusState
    _ -> pure ()

-- | Perform a DMA transfer.
{-# INLINABLE doDMA #-}
doDMA :: HasMemory env => BusEvent -> ReaderT env IO ()
doDMA busEvent = when (DMA `elem` writeAddress busEvent) $ do
  address <- fromIntegral <$> readByte DMA
  dmaToOAM (address `shiftL` 8)

-- | Execute a single step of the emulation.
{-# INLINABLE busStep #-}
busStep :: HasBus env => ReaderT env IO BusEvent
busStep = do
  busEvent <- cpuStep
  keypadHandleBusEvent busEvent
  doDMA busEvent

  BusState {..} <- asks forBusState
  now           <- SDL.ticks
  lastPoll      <- liftIO $ readIORef lastEventPollAt
  when (now - lastPoll > pollDelay) $ do
    handleEvents
    liftIO $ writeIORef lastEventPollAt =<< SDL.ticks

  graphicsStep busEvent
  pure busEvent
