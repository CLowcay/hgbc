{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Primitive.Counter
  ( Counter,
    new,
    get,
    reload,
    update,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits (Bits (..))
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)

-- | A reloading down counter.  Number of states is reload value + 1.
data Counter = Counter
  { counterMask :: !Int,
    counterValue :: !(UnboxedRef Int)
  }

new :: MonadIO m => Int -> m Counter
new counterMask = do
  counterValue <- newUnboxedRef 0
  pure Counter {..}

{-# INLINE reload #-}
reload :: MonadIO m => Counter -> Int -> m ()
reload Counter {..} value =
  writeUnboxedRef counterValue (if value == 0 then counterMask + 1 else value)

{-# INLINE get #-}
get :: MonadIO m => Counter -> m Int
get Counter {..} = do
  v <- readUnboxedRef counterValue
  pure (v .&. counterMask)

{-# INLINE update #-}
update :: MonadIO m => Counter -> Int -> m Int -> m ()
update Counter {..} advance getReloadValue = do
  count <- readUnboxedRef counterValue
  if advance < count
    then writeUnboxedRef counterValue (count - advance)
    else do
      value <- getReloadValue
      let period = if value == 0 then counterMask + 1 else value
      writeUnboxedRef counterValue (count - advance + period)
