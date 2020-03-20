module SDL.Extras
  ( DisplayIndex
  , getWindowDisplayIndex
  , getCurrentDisplayMode
  )
where

import           Control.Monad.IO.Class
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           SDL.Internal.Types
import qualified SDL.Raw

newtype DisplayIndex = DisplayIndex CInt deriving (Eq, Ord, Show)

getWindowDisplayIndex :: MonadIO m => Window -> m DisplayIndex
getWindowDisplayIndex (Window window) = DisplayIndex <$> SDL.Raw.getWindowDisplayIndex window

getCurrentDisplayMode :: MonadIO m => DisplayIndex -> m (Maybe SDL.Raw.DisplayMode)
getCurrentDisplayMode (DisplayIndex index) = liftIO $ alloca $ \pDisplayMode -> do
  r <- SDL.Raw.getCurrentDisplayMode index pDisplayMode
  if r < 0 then pure Nothing else Just <$> peek pDisplayMode
