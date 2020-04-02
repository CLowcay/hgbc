module Emulator
  ( Notification(..)
  , Emulator
  , new
  , sendNotification
  , getNotification
  , waitNotification
  )
where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class

-- | A notification for the emulator thread.
data Notification
  = PauseNotification
  | QuitNotification
  | RunNotification
  | StepNotification
  | StepOverNotification
  | StepOutNotification
  deriving (Eq, Ord, Show)

-- | A communication channel for the emulator thread.
newtype Emulator = Emulator (TChan Notification)

-- | Create a new 'Emulator'.
new :: IO Emulator
new = Emulator <$> newTChanIO

-- | Send a 'Notification' to an 'Emulator'.
sendNotification :: MonadIO m => Emulator -> Notification -> m ()
sendNotification (Emulator emulator) = liftIO . atomically . writeTChan emulator

-- | Receieve a 'Notification' if there is one waiting.
getNotification :: MonadIO m => Emulator -> m (Maybe Notification)
getNotification (Emulator emulator) = liftIO $ atomically $ tryReadTChan emulator

-- | Block until a 'Notification' arrives.
waitNotification :: MonadIO m => Emulator -> m Notification
waitNotification (Emulator emulator) = liftIO $ atomically $ readTChan emulator
