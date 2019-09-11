{-# LANGUAGE TemplateHaskell #-}
module GBC.Bus
  ( BusState(..)
  , Bus
  , cpu
  , graphics
  , registerGraphicsSynchronizer
  , busStep
  )
where

import           GBC.CPU
import           GBC.Graphics
import           GBC.Bus.Synchronizer
import           Control.Lens
import           Control.Monad.State.Strict
import           GBC.Memory
import           Control.Monad.Reader
import           System.Mem.Weak

data BusState = BusState {
    _cpu :: !CPUState
  , _graphics :: !GraphicsState
  , _graphicsSync :: !(SynchronizerSet Update)
}
makeLenses ''BusState

type Bus a = ReaderT Memory (StateT BusState IO) a

registerGraphicsSynchronizer :: Synchronizer Update -> Bus ()
registerGraphicsSynchronizer synchronizer = do
  sync <- liftIO $ mkWeakPtr synchronizer Nothing
  graphicsSync %= (sync : )

busStep :: Bus (BusEvent, Maybe Update)
busStep = do
  event   <- zoom cpu cpuStep
  mupdate <- zoom graphics $ graphicsStep event
  case mupdate of
    Nothing     -> pure ()
    Just update -> do
      sync' <- (liftIO . broadcastUpdate update) =<< use graphicsSync
      graphicsSync .= sync'
  pure (event, mupdate)
