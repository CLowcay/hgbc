module Machine.GBC.Mode
  ( EmulatorMode(..)
  , cgbOnlyPort
  )
where

import           Machine.GBC.Primitive
import           Data.IORef
import           Data.Word

data EmulatorMode = DMG | CGB deriving (Eq, Ord, Show, Bounded, Enum)

cgbOnlyPort
  :: IORef EmulatorMode -> Word8 -> Word8 -> (Word8 -> Word8 -> IO Word8) -> IO Port
cgbOnlyPort modeRef v0 writeMask writeFunction = newPortWithReadAction
  v0
  writeMask
  (\v -> do
    m <- readIORef modeRef
    pure $ case m of
      DMG -> 0xFF
      CGB -> v
  )
  (\v v' -> do
    m <- readIORef modeRef
    case m of
      DMG -> pure v
      CGB -> writeFunction v v'
  )
