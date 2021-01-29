{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Primitive.RingBuffer
  ( RingBuffer,
    new,
    readableSize,
    writableSize,
    write,
    fold,
  )
where

import Control.Monad (when)
import Data.Bits (Bits (..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Storable (Storable (..))
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)

data RingBuffer a = RingBuffer
  { ringMask :: {-# UNPACK #-} !Int,
    ringBuffer :: !(ForeignPtr a),
    ringReadPtr :: !(UnboxedRef Int),
    ringWritePtr :: !(UnboxedRef Int)
  }

new :: Storable a => Int -> IO (RingBuffer a)
new size = do
  ringBuffer <- mallocForeignPtrArray (2 ^ size)
  ringReadPtr <- newUnboxedRef 0
  ringWritePtr <- newUnboxedRef 0
  let ringMask = (2 ^ size) - 1
  pure RingBuffer {..}

{-# INLINE readableSize #-}
readableSize :: RingBuffer a -> IO Int
readableSize RingBuffer {..} = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  pure $ writePtr - readPtr

{-# INLINE writableSize #-}
writableSize :: RingBuffer a -> IO Int
writableSize RingBuffer {..} = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  pure $ (ringMask + 1) - (writePtr - readPtr)

{-# INLINE write #-}
write :: Storable a => RingBuffer a -> a -> IO ()
write RingBuffer {..} x = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  when ((writePtr - readPtr) <= ringMask) $ do
    withForeignPtr ringBuffer $ \ptr -> pokeElemOff ptr (writePtr .&. ringMask) x
    writeUnboxedRef ringWritePtr (writePtr + 1)

{-# INLINEABLE fold #-}
fold :: Storable a => RingBuffer a -> Int -> b -> (b -> a -> IO b) -> IO b
fold RingBuffer {..} limit acc0 accumulate = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  let nextReadPtr = writePtr `min` (readPtr + limit)
  writeUnboxedRef ringReadPtr nextReadPtr
  withForeignPtr ringBuffer $ \base ->
    let go !acc !i =
          if i == nextReadPtr
            then pure acc
            else do
              acc' <- accumulate acc =<< peekElemOff base (i .&. ringMask)
              go acc' (i + 1)
     in go acc0 readPtr