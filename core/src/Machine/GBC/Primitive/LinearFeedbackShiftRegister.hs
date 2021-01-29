module Machine.GBC.Primitive.LinearFeedbackShiftRegister
  ( LinearFeedbackShiftRegister,
    new,
    init,
    nextBit,
    currentBit,
  )
where

import Data.Bits (Bits (..))
import Data.Primitive (Prim (..))
import Data.Word (Word8)
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import Prelude hiding (init)

newtype LinearFeedbackShiftRegister a = LinearFeedbackShiftRegister (UnboxedRef a)

new :: (Prim a, Bits a) => IO (LinearFeedbackShiftRegister a)
new = LinearFeedbackShiftRegister <$> newUnboxedRef (bit 0)

{-# INLINE init #-}
{-# SPECIALIZE init :: Word8 -> LinearFeedbackShiftRegister Word8 -> IO () #-}
init :: Prim a => a -> LinearFeedbackShiftRegister a -> IO ()
init value (LinearFeedbackShiftRegister ref) = writeUnboxedRef ref value

{-# INLINE nextBit #-}
{-# SPECIALIZE nextBit :: LinearFeedbackShiftRegister Word8 -> Word8 -> IO Word8 #-}
nextBit :: (Prim a, Num a, Bits a) => LinearFeedbackShiftRegister a -> a -> IO a
nextBit (LinearFeedbackShiftRegister ref) mask = do
  register <- readUnboxedRef ref
  let shifted = register `unsafeShiftR` 1
  let register' =
        (shifted .&. complement mask) .|. (mask .&. negate (1 .&. (register `xor` shifted)))
  writeUnboxedRef ref register'
  pure register'

{-# INLINE currentBit #-}
currentBit :: (Prim a) => LinearFeedbackShiftRegister a -> IO a
currentBit (LinearFeedbackShiftRegister ref) = readUnboxedRef ref