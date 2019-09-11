{-# LANGUAGE RecordWildCards #-}

module GBC.Bus.Synchronizer
  ( Synchronizer
  , SynchronizerSet
  , newSynchronizer
  , sendUpdate
  , broadcastUpdate
  , getUpdate
  , notifyThreadTerminated
  )
where

import           Control.Concurrent.MVar
import           Control.Concurrent.Async
import           System.Mem.Weak
import           Control.Monad
import           Data.Maybe

-- | An object to synchronize the actual display windows with the rest of the
-- simulation.
data Synchronizer a = Synchronizer {
    updateInfo :: !(MVar a)   -- ^ Write an 'Update' here to send the update to the window.
  , updateAck :: !(MVar ())        -- ^ Wait for an acknowledgement to show up here.
  , threadGone :: !(MVar ())       -- ^ If this is not empty then the synchronized thread is dead.
}

-- | Initialize a new 'Synchronizer'.
newSynchronizer :: IO (Synchronizer a)
newSynchronizer = Synchronizer <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar

-- | Send an update to a 'Synchronizer'.
sendUpdate :: Synchronizer a -> a -> IO ()
sendUpdate Synchronizer {..} update = do
  void $ tryPutMVar updateInfo update
  race_ (takeMVar updateAck) (swapMVar threadGone ())

-- | Get an update from a 'Synchronizer'.
getUpdate :: Synchronizer a -> IO (Maybe a)
getUpdate Synchronizer {..} = do
  r <- tryTakeMVar updateInfo
  when ( isJust r) $ putMVar updateAck ()
  pure r

-- | Notify the 'Synchronizer' that the thread has terminated.
notifyThreadTerminated :: Synchronizer a -> IO ()
notifyThreadTerminated Synchronizer {..} = void $ tryPutMVar threadGone ()

-- | A set of 'Synchronizer's.
type SynchronizerSet a = [Weak (Synchronizer a)]

-- | Broadcast an update to a set of 'Synchronizer's.
broadcastUpdate :: a -> SynchronizerSet a -> IO (SynchronizerSet a)
broadcastUpdate update = foldM
  (\seen next -> do
    v <- deRefWeak next
    case v of
      Nothing           -> putStrLn "DEAD" >> pure seen
      Just synchronizer -> do
        sendUpdate synchronizer update
        pure $ next : seen
  )
  []
