{-# LANGUAGE TemplateHaskell #-}
module GBC.Bus
  ( BusState(..)
  , Bus
  , cpu
  , graphics
  , busStep
  )
where

import           GBC.CPU
import           GBC.Graphics
import           Control.Lens
import           Control.Monad.State.Strict
import           GBC.Memory
import           Control.Monad.Reader

data BusState = BusState {
    _cpu :: !CPUState
  , _graphics :: !GraphicsState
}
makeLenses ''BusState

type Bus a = ReaderT Memory (StateT BusState IO) a

busStep :: Bus (BusEvent, Maybe Update)
busStep = do
  event  <- zoom cpu cpuStep
  update <- zoom graphics $ graphicsStep event
  pure (event, update)
