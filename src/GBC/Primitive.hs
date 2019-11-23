module GBC.Primitive
  ( Counter
  , newCounter
  , reloadCounter
  , updateCounter
  , StateCycle
  , newStateCycle
  , getStateCycle
  , updateStateCycle
  , resetStateCycle
  )
where

import           Data.IORef
import           Control.Monad.IO.Class
import           Control.Monad.Fail

newtype Counter = Counter (IORef Int)

newCounter :: MonadIO m => m Counter
newCounter = liftIO $ Counter <$> newIORef 0

{-# INLINE reloadCounter #-}
reloadCounter :: MonadIO m => Int -> Counter -> m ()
reloadCounter reload (Counter ref) = liftIO $ writeIORef ref reload

{-# INLINE updateCounter #-}
updateCounter :: MonadIO m => Counter -> Int -> m Int -> m ()
updateCounter (Counter ref) update k = do
  count <- liftIO $ readIORef ref
  let count' = count - update
  if count' > 0
    then liftIO $ writeIORef ref count'
    else do
      reload <- k
      liftIO $ writeIORef ref (count' + reload)

data StateCycle a = StateCycle !(IORef Int) !(IORef [(a, Int)])

newStateCycle :: MonadIO m => [(a, Int)] -> m (StateCycle a)
newStateCycle [] = error "Tried to create a counter with no states!"
newStateCycle states@((_, count0) : _) =
  liftIO $ StateCycle <$> newIORef count0 <*> newIORef (cycle states)

{-# INLINE getStateCycle #-}
getStateCycle :: (MonadIO m, MonadFail m) => StateCycle a -> m a
getStateCycle (StateCycle _ states) = do
  ((state, _) : _) <- liftIO $ readIORef states
  pure state

{-# INLINE updateStateCycle #-}
updateStateCycle :: MonadIO m => StateCycle a -> Int -> (a -> m ()) -> m a
updateStateCycle (StateCycle cycles states) update k = do
  count <- liftIO $ readIORef cycles
  let count' = count - update
  if count' > 0
    then liftIO $ do
      writeIORef cycles count'
      fst . head <$> readIORef states
    else do
      stateList <- liftIO $ readIORef states
      let stateList'                   = tail stateList
      let (nextState, nextStateLength) = head stateList'
      liftIO $ writeIORef states stateList'
      liftIO $ writeIORef cycles (count' + nextStateLength)
      k nextState
      pure nextState

{-# INLINE resetStateCycle #-}
resetStateCycle :: MonadIO m => StateCycle a -> [(a, Int)] -> m ()
resetStateCycle (StateCycle cycles states) states' = case states' of
  []                -> error "Tried to reset a counter with no states!"
  ((_, count0) : _) -> liftIO $ do
    writeIORef states (cycle states')
    writeIORef cycles count0
