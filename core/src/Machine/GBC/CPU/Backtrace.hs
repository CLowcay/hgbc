{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.CPU.Backtrace
  ( Backtrace,
    new,
    reset,
    push,
    pop,
    toList,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits (Bits (..))
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word16)
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)

data Backtrace = Backtrace
  { trace :: !(VUM.IOVector (Word16, Word16)),
    mask :: Int,
    topRef :: !(UnboxedRef Int),
    bottomRef :: !(UnboxedRef Int)
  }

{-# INLINE new #-}
new :: MonadIO m => Int -> m Backtrace
new size = do
  let mask = (2 ^ size) - 1
  trace <- liftIO (VUM.new (2 ^ size))
  topRef <- newUnboxedRef 0
  bottomRef <- newUnboxedRef 0
  pure Backtrace {..}

reset :: MonadIO m => Backtrace -> m ()
reset Backtrace {..} = do
  writeUnboxedRef topRef 0
  writeUnboxedRef bottomRef 0

{-# INLINE push #-}
push :: MonadIO m => Backtrace -> Word16 -> Word16 -> m ()
push Backtrace {..} bank offset = do
  top <- readUnboxedRef topRef
  bottom <- readUnboxedRef bottomRef
  let top' = (top + 1) .&. mask
  writeUnboxedRef topRef top'
  when (top' == bottom) $ writeUnboxedRef bottomRef ((bottom + 1) .&. mask)
  liftIO (VUM.write trace top (bank, offset))

{-# INLINE pop #-}
pop :: MonadIO m => Backtrace -> m ()
pop Backtrace {..} = do
  top <- readUnboxedRef topRef
  bottom <- readUnboxedRef bottomRef
  when (top /= bottom) $ writeUnboxedRef topRef ((top - 1) .&. mask)

toList :: MonadIO m => Backtrace -> m [(Word16, Word16)]
toList Backtrace {..} = do
  top <- readUnboxedRef topRef
  bottom <- readUnboxedRef bottomRef
  traverse (liftIO . VUM.read trace) (takeWhile (/= top) [(bottom + i) .&. mask | i <- [0 ..]])
