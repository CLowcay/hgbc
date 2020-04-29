{-# LANGUAGE OverloadedStrings #-}

module HGBC.Events
  ( Event(..)
  , Channel
  , newChannel
  , send
  , waitAction
  )
where

import           Control.Concurrent.STM
import           Control.Exception              ( IOException )
import           Control.Monad.IO.Class
import           Machine.GBC.Disassembler

data Event
  = Resumed
  | Paused
  | Statistics Double Int          -- ^ Time in seconds * emulator clock.
  | IOWarning String IOException   -- ^ A non-fatal IO error occured.
  | BreakPointSet LongAddress
  | BreakPointDisabled LongAddress
  | BreakPointRemoved LongAddress
  | LabelUpdated Labels
  | LabelRemoved LongAddress
  deriving (Eq, Show)

newtype Channel = Channel (TChan Event)

-- | Create a new 'DebuggerChannel'.
newChannel :: IO Channel
newChannel = Channel <$> newBroadcastTChanIO

-- | Send a notification to the event channel.
send :: MonadIO m => Channel -> Event -> m ()
send (Channel channel) = liftIO . atomically . writeTChan channel

-- | Get an action that reads from the event channel.
waitAction :: MonadIO m => Channel -> m (IO Event)
waitAction (Channel writeChannel) = do
  channel <- liftIO (atomically (dupTChan writeChannel))
  pure (atomically (readTChan channel))
