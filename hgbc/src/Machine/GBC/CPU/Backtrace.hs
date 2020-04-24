{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.CPU.Backtrace
  ( Backtrace
  , new
  , push
  , pop
  , toList
  )
where

import           Control.Monad
import           Data.Bits
import           Data.Word
import           Machine.GBC.Primitive.UnboxedRef
import qualified Data.Vector.Unboxed.Mutable   as VUM

data Backtrace = Backtrace {
    trace      :: !(VUM.IOVector (Word16, Word16))
  , mask       :: Int
  , topRef     :: !(UnboxedRef Int)
  , bottomRef  :: !(UnboxedRef Int)
}

{-# INLINE new #-}
new :: Int -> IO Backtrace
new size = do
  let mask = (2 ^ size) - 1
  trace     <- VUM.new (2 ^ size)
  topRef    <- newUnboxedRef 0
  bottomRef <- newUnboxedRef 0
  pure Backtrace { .. }

{-# INLINE push #-}
push :: Backtrace -> Word16 -> Word16 -> IO ()
push Backtrace {..} bank offset = do
  top    <- readUnboxedRef topRef
  bottom <- readUnboxedRef bottomRef
  let top' = top + 1 .&. mask
  writeUnboxedRef topRef top'
  when (top' == bottom) $ writeUnboxedRef bottomRef (bottom + 1)
  VUM.write trace top (bank, offset)

{-# INLINE pop #-}
pop :: Backtrace -> IO ()
pop Backtrace {..} = do
  top    <- readUnboxedRef topRef
  bottom <- readUnboxedRef bottomRef
  when (top /= bottom) $ writeUnboxedRef topRef (top - 1)

toList :: Backtrace -> IO [(Word16, Word16)]
toList Backtrace {..} = do
  top    <- readUnboxedRef topRef
  bottom <- readUnboxedRef bottomRef
  traverse (VUM.read trace) (takeWhile (/= top) [ bottom + i .&. mask | i <- [0 ..] ])
