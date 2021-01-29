module Machine.GBC.Primitive.StateCycle
  ( UpdateResult (..),
    StateCycle,
    new,
    getState,
    getUpdateResult,
    update,
    reset,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)

data StateCycle a = StateCycle !(UnboxedRef Int) !(IORef [(a, Int)])

new :: MonadIO m => [(a, Int)] -> m (StateCycle a)
new [] = error "Tried to create a counter with no states!"
new states@((_, count0) : _) =
  StateCycle <$> newUnboxedRef count0 <*> liftIO (newIORef (cycle states))

{-# INLINE getState #-}
getState :: (MonadIO m, MonadFail m) => StateCycle a -> m a
getState (StateCycle _ states) = do
  ((state, _) : _) <- liftIO (readIORef states)
  pure state

data UpdateResult a = NoChange !a | HasChangedTo !a deriving (Eq, Ord, Show)

getUpdateResult :: UpdateResult a -> a
getUpdateResult (NoChange x) = x
getUpdateResult (HasChangedTo x) = x

{-# INLINE update #-}
update :: MonadIO m => StateCycle a -> Int -> (a -> m ()) -> m (UpdateResult a)
update (StateCycle cycles states) advance k = do
  count <- readUnboxedRef cycles
  let count' = count - advance
  if count' > 0
    then do
      writeUnboxedRef cycles count'
      NoChange . fst . head <$> liftIO (readIORef states)
    else do
      stateList <- liftIO $ readIORef states
      let stateList' = tail stateList
      let (nextState, nextStateLength) = head stateList'
      liftIO (writeIORef states $! stateList')
      writeUnboxedRef cycles (count' + nextStateLength)
      k nextState
      pure (HasChangedTo nextState)

{-# INLINE reset #-}
reset :: MonadIO m => StateCycle a -> [(a, Int)] -> m ()
reset (StateCycle cycles states) states' = case states' of
  [] -> error "Tried to reset a counter with no states!"
  ((_, count0) : _) -> do
    liftIO (writeIORef states $! cycle states')
    writeUnboxedRef cycles count0
