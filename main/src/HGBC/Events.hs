{-# LANGUAGE OverloadedStrings #-}

module HGBC.Events
  ( Event (..),
    Channel,
    newChannel,
    send,
    waitAction,
  )
where

import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (IOException)
import Control.Monad.IO.Class (MonadIO (..))
import Machine.GBC.Disassembler (Labels, LongAddress)
import qualified Machine.GBC.Errors as GBC

data Event
  = Resumed
  | Paused
  | Fault GBC.Fault
  | -- | Time in seconds * emulator clock.
    Statistics Double Int
  | -- | A non-fatal IO error occured.
    IOWarning String IOException
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
