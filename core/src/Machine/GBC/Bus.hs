module Machine.GBC.Bus
  ( Has(..)
  )
where

import           Data.Word
import           Control.Monad.Reader

class Has env where
  -- | Do one read cycle.
  read :: Word16 -> ReaderT env IO Word8
  -- | Do one write cycle.
  write :: Word16 -> Word8 -> ReaderT env IO ()
  -- | Delay one cycle.
  delay :: ReaderT env IO ()
  -- | Delay measured in clocks.
  delayClocks :: Int -> ReaderT env IO ()
