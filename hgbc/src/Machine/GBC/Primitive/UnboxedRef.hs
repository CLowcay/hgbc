module Machine.GBC.Primitive.UnboxedRef
  ( UnboxedRef
  , newUnboxedRef
  , writeUnboxedRef
  , readUnboxedRef
  )
where

import           GHC.Exts
import           Data.Primitive

newtype UnboxedRef a = UnboxedRef (MutablePrimArray RealWorld a)

{-# INLINE newUnboxedRef #-}
newUnboxedRef :: Prim a => a -> IO (UnboxedRef a)
newUnboxedRef value = do
  array <- newPrimArray 1
  writePrimArray array 0 value
  pure (UnboxedRef array)

{-# INLINE writeUnboxedRef #-}
writeUnboxedRef :: Prim a => UnboxedRef a -> a -> IO ()
writeUnboxedRef (UnboxedRef array) = writePrimArray array 0

{-# INLINE readUnboxedRef #-}
readUnboxedRef :: Prim a => UnboxedRef a -> IO a
readUnboxedRef (UnboxedRef array) = readPrimArray array 0
