{-# LANGUAGE RecordWildCards #-}
module Window
  ( Window
  , Notification(..)
  , sdlWindow
  , new
  , sendNotification
  , dispatchNotification
  )
where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Int
import           Prelude                 hiding ( lookup )
import qualified SDL

-- | Notification of a window event.
data Notification
  = CloseNotification                      -- ^ The window was closed.
  | PausedNotification                     -- ^ The emulator has been paused.
  | ResumedNotification                    -- ^ The emulator has been resumed.
  | SizeChangedNotification (SDL.V2 Int32) -- ^ The window size was changed.
  | MovedNotification (SDL.Point SDL.V2 Int32) -- ^ The window was moved.
  deriving (Eq, Ord, Show)

instance Exception Notification

-- | A wrapper over an SDL 'SDL.Window'.
data Window = Window
  { sdlWindow :: SDL.Window
  , threadId  :: ThreadId
  }

-- | Create a new 'Window' from an 'SDL.Window'.
new :: SDL.Window -> ThreadId -> Window
new sdlWindow threadId = Window { .. }

-- | Send a 'Notification' to a window.
sendNotification :: MonadIO m => Window -> Notification -> m ()
sendNotification window = liftIO . throwTo (threadId window)

-- | Convert an SDL 'SDL.Event. into a 'Notification' and dispatch it to the
-- appropriate 'Window' given a function to lookup the window.
dispatchNotification :: MonadIO m => (SDL.Window -> Maybe Window) -> SDL.EventPayload -> m ()
dispatchNotification lookup event = case event of
  (SDL.WindowClosedEvent (SDL.WindowClosedEventData window)) ->
    maybeSendNotification (lookup window) CloseNotification
  (SDL.WindowSizeChangedEvent (SDL.WindowSizeChangedEventData window size)) ->
    maybeSendNotification (lookup window) (SizeChangedNotification size)
  (SDL.WindowMovedEvent (SDL.WindowMovedEventData window position)) ->
    maybeSendNotification (lookup window) (MovedNotification position)
  _ -> pure ()
 where
  maybeSendNotification Nothing       _            = pure ()
  maybeSendNotification (Just window) notification = sendNotification window notification
