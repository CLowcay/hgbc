{-# LANGUAGE RecordWildCards #-}
module GBC.Audio.Length
  ( Length
  , newLength
  , initLength
  , reloadLength
  , clockLength
  )
where

import           GBC.Primitive
import           Data.Word
import           Data.Bits
import           Control.Monad

data Length = Length {
    bitMask :: Word8
  , counter :: Counter
}

newLength :: Word8 -> IO Length
newLength bitMask = do
  counter <- newCounter
  pure Length { .. }

{-# INLINE initLength #-}
initLength :: Length -> IO ()
initLength Length {..} = do
  l <- getCounter counter
  when (l == 0) $ reloadCounter counter (fromIntegral bitMask + 1)

{-# INLINE reloadLength #-}
reloadLength :: Length -> Word8 -> IO ()
reloadLength Length {..} register =
  reloadCounter counter (fromIntegral bitMask .&. negate (fromIntegral (register .&. bitMask)))

{-# INLINE clockLength #-}
clockLength :: Length -> IO () -> IO ()
clockLength Length {..} action = updateCounter counter 1 $ 0 <$ action
