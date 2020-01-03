module Debug.Breakpoints
  ( BreakpointTable
  , initBreakpointTable
  , setBreakpoint
  , getBreakpoint
  , shouldBreakOnExecute
--  , shouldBreakOnWrite
  , clearBreakpoint
  , listBreakpoints
  )
where

import           Control.Monad.Reader
import           Data.HashTable.IO
import           Data.Word
import           Data.Maybe
import           GBC.CPU
import           Prelude                 hiding ( lookup )

type BreakpointEnabled = Bool

type BreakpointTable = BasicHashTable Word16 BreakpointEnabled

initBreakpointTable :: IO BreakpointTable
initBreakpointTable = new

setBreakpoint :: BreakpointTable -> Word16 -> BreakpointEnabled -> IO ()
setBreakpoint = insert

getBreakpoint :: BreakpointTable -> Word16 -> IO (Maybe BreakpointEnabled)
getBreakpoint = lookup

shouldBreakOnExecute :: HasCPU env => BreakpointTable -> ReaderT env IO Bool
shouldBreakOnExecute table = do
  pc <- readPC
  liftIO $ (== Just True) <$> lookup table pc

-- TODO: fix watch points.
--shouldBreakOnWrite table (BusEvent writes _ _) =
--  liftIO $ or . catMaybes <$> traverse (lookup table) writes

clearBreakpoint :: BreakpointTable -> Word16 -> IO ()
clearBreakpoint = delete

listBreakpoints :: BreakpointTable -> IO [(Word16, BreakpointEnabled)]
listBreakpoints = toList
