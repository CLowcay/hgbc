{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module GBC.Bus
  ( BusState(..)
  , Bus
  , cpu
  , initBusState
  , registerWindow
  , busStep
  , handleEvents
  )
where

import           GBC.CPU
import qualified GBC.Keypad                    as Keypad
import           GBC.Graphics
import           Control.Lens
import           Control.Monad.State.Strict
import           GBC.Memory
import           Control.Monad.Reader
import           Control.Concurrent.MVar
import           SDL
import qualified Data.HashTable.IO             as H
import           Data.Foldable
import           SDL.Orphans                    ( )

data BusState = BusState {
    _cpu :: !CPUState
  , _graphics :: !GraphicsState
  , _graphicsOutput :: !(H.BasicHashTable Window (MVar (Maybe Update)))
  , _keypadState :: !Keypad.KeypadState
}
makeLenses ''BusState

initBusState :: CPUState -> IO BusState
initBusState cpuState = do
  output <- initOutput
  pure $ BusState cpuState initGraphics output Keypad.initKeypadState

initOutput :: IO (H.BasicHashTable Window (MVar (Maybe Update)))
initOutput = H.new

type Bus a = ReaderT Memory (StateT BusState IO) a

registerWindow :: Window -> MVar (Maybe Update) -> Bus ()
registerWindow window queue = do
  windows <- use graphicsOutput
  liftIO $ H.insert windows window queue

killWindow :: Window -> Bus ()
killWindow window = do
  windows <- use graphicsOutput
  liftIO $ do
    mvar <- H.lookup windows window
    case mvar of
      Nothing  -> pure ()
      Just var -> do
        putMVar var Nothing
        H.delete windows window

handleEvents :: Bus ()
handleEvents = do
  events <- pollEvents
  zoom keypadState $ Keypad.processEvents events
  for_ (eventPayload <$> events) $ \case
    (WindowClosedEvent d) -> killWindow (windowClosedEventWindow d)
    _                     -> pure ()

busStep :: Bus (BusEvent, Maybe Update)
busStep = do
  busEvent <- zoom cpu cpuStep
  zoom keypadState $ Keypad.processBusEvent busEvent

  handleEvents

  graphicsUpdate <- zoom graphics $ graphicsStep busEvent
  case graphicsUpdate of
    Nothing     -> pure ()
    Just update -> do
      windows <- use graphicsOutput
      liftIO $ traverse_ ((`putMVar` Just update) . snd) =<< H.toList windows
  pure (busEvent, graphicsUpdate)
