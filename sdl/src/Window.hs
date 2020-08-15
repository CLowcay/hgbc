{-# LANGUAGE RecordWildCards #-}

module Window
  ( Window,
    Notification (..),
    sdlWindow,
    new,
    send,
    dispatchEvent,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Int
import qualified SDL
import Prelude hiding (lookup)

-- | Notification of a window event.
data Notification
  = -- | The window was closed.
    Close
  | -- | The emulator has been paused.
    Paused
  | -- | The emulator has stopped after a fault.
    Fault
  | -- | The emulator has been resumed.
    Resumed
  | -- | The window size was changed.
    SizeChanged (SDL.V2 Int32)
  | -- | The window was moved.
    Moved (SDL.Point SDL.V2 Int32)
  deriving (Eq, Ord, Show)

instance Exception Notification

-- | A wrapper over an SDL 'SDL.Window'.
data Window = Window
  { sdlWindow :: SDL.Window,
    threadId :: ThreadId
  }

-- | Create a new 'Window' from an 'SDL.Window'.
new :: SDL.Window -> ThreadId -> Window
new sdlWindow threadId = Window {..}

-- | Send a 'Notification' to a window.
send :: MonadIO m => Window -> Notification -> m ()
send window = liftIO . throwTo (threadId window)

-- | Convert an SDL 'SDL.Event. into a 'Notification' and dispatch it to the
-- appropriate 'Window' given a function to lookup the window.
dispatchEvent :: MonadIO m => (SDL.Window -> Maybe Window) -> SDL.EventPayload -> m ()
dispatchEvent lookup event = case event of
  (SDL.WindowClosedEvent (SDL.WindowClosedEventData window)) -> maybeSend (lookup window) Close
  (SDL.WindowSizeChangedEvent (SDL.WindowSizeChangedEventData window size)) ->
    maybeSend (lookup window) (SizeChanged size)
  (SDL.WindowMovedEvent (SDL.WindowMovedEventData window position)) ->
    maybeSend (lookup window) (Moved position)
  _ -> pure ()
  where
    maybeSend Nothing _ = pure ()
    maybeSend (Just window) notification = send window notification
