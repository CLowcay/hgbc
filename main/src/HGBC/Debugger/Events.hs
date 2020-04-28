{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.Events
  ( Notification(..)
  , Channel
  , newChannel
  , send
  , stream
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.String
import           HGBC.Debugger.Status
import           Machine.GBC.CPU                ( readPC )
import           Machine.GBC.Disassembler
import           Machine.GBC.Memory             ( getBank )
import qualified Control.Concurrent.Async      as Async
import qualified Data.ByteString.Builder       as BB
import qualified HGBC.Debugger.JSON            as JSON
import qualified Machine.GBC                   as GBC

data Notification
  = EmulatorStarted
  | EmulatorPaused
  | BreakPointSet LongAddress
  | BreakPointDisabled LongAddress
  | BreakPointRemoved LongAddress
  | LabelUpdated Labels
  | LabelRemoved LongAddress
  deriving (Eq, Ord, Show)

newtype Channel = Channel (TChan Notification)

-- | Create a new 'DebuggerChannel'.
newChannel :: IO Channel
newChannel = Channel <$> newBroadcastTChanIO

-- | Send a notification to the event channel.
send :: MonadIO m => Channel -> Notification -> m ()
send (Channel channel) = liftIO . atomically . writeTChan channel

-- | Minimum frequency to send a keep-alive event.
keepAliveTime :: Int
keepAliveTime = 60 * 1000000

-- | Delay between updates.
updateDelay :: Int
updateDelay = 250000

stream :: Channel -> GBC.EmulatorState -> (BB.Builder -> IO ()) -> IO () -> IO ()
stream (Channel writeChannel) emulatorState write flush = do
  channel <- atomically (dupTChan writeChannel)
  let continue isPaused = do
        event <- Async.race (threadDelay (if isPaused then keepAliveTime else updateDelay))
                            (atomically (readTChan channel))
        case event of
          Left () -> do
            if isPaused then write ": keep-alive\n\n" >> flush else pushStatus
            continue isPaused
          Right EmulatorStarted -> do
            write "event: started\ndata:\n\n" >> flush
            continue False
          Right EmulatorPaused -> do
            pushPaused
            continue True
          Right (BreakPointSet address) -> do
            write "event: breakpoint-added\ndata:"
            pushAddress address
            continue isPaused
          Right (BreakPointDisabled address) -> do
            write "event: breakpoint-disabled\ndata:"
            pushAddress address
            continue isPaused
          Right (BreakPointRemoved address) -> do
            write "event: breakpoint-removed\ndata:"
            pushAddress address
            continue isPaused
          Right (LabelUpdated labels) -> do
            write ("event: label-added\ndata:" <> fromEncoding (JSON.labels labels) <> "\n\n")
            flush
            continue isPaused
          Right (LabelRemoved address) -> do
            write "event: label-removed\ndata:"
            pushAddress address
            continue isPaused

  pushPaused >> continue True

 where
  pushStatus = do
    status <- getStatus emulatorState
    write ("event: status\ndata:" <> BB.lazyByteString status <> "\n\n") >> flush
  pushPaused = do
    pushStatus
    (bank, pc) <- runReaderT
      (do
        pc'   <- readPC
        bank' <- getBank pc'
        pure (bank', pc')
      )
      emulatorState
    write
        (  "event: paused\ndata:{\"bank\":"
        <> fromString (show bank)
        <> ",\"offset\":"
        <> fromString (show pc)
        <> "}\n\n"
        )
      >> flush
  pushAddress address = write (fromEncoding (JSON.longAddress address) <> "\n\n") >> flush
