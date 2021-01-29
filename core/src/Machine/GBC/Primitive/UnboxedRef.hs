module Machine.GBC.Primitive.UnboxedRef
  ( UnboxedRef,
    newUnboxedRef,
    writeUnboxedRef,
    readUnboxedRef,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Primitive (MutablePrimArray, Prim, newPrimArray, readPrimArray, writePrimArray)
import GHC.Exts (RealWorld)

newtype UnboxedRef a = UnboxedRef (MutablePrimArray RealWorld a)

{-# INLINE newUnboxedRef #-}
newUnboxedRef :: (MonadIO m, Prim a) => a -> m (UnboxedRef a)
newUnboxedRef value = liftIO $ do
  array <- newPrimArray 1
  writePrimArray array 0 value
  pure (UnboxedRef array)

{-# INLINE writeUnboxedRef #-}
writeUnboxedRef :: (MonadIO m, Prim a) => UnboxedRef a -> a -> m ()
writeUnboxedRef (UnboxedRef array) v = liftIO (writePrimArray array 0 v)

{-# INLINE readUnboxedRef #-}
readUnboxedRef :: (MonadIO m, Prim a) => UnboxedRef a -> m a
readUnboxedRef (UnboxedRef array) = liftIO (readPrimArray array 0)
