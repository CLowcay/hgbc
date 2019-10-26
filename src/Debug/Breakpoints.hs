module Debug.Breakpoints
  ( BreakpointTable
  , initBreakpointTable
  , setBreakpoint
  , getBreakpoint
  , shouldBreak
  , clearBreakpoint
  , listBreakpoints
  )
where

import           Control.Monad.Reader
import           Data.HashTable.IO
import           Data.Word
import           GBC.CPU
import           Prelude                 hiding ( lookup )

type BreakpointTable = BasicHashTable Word16 Bool

initBreakpointTable :: IO BreakpointTable
initBreakpointTable = new

setBreakpoint :: BreakpointTable -> Word16 -> Bool -> IO ()
setBreakpoint = insert

getBreakpoint :: BreakpointTable -> Word16 -> IO (Maybe Bool)
getBreakpoint = lookup

shouldBreak :: HasCPU env => BreakpointTable -> ReaderT env IO Bool
shouldBreak table = do
  pc <- readPC
  liftIO $ (== Just True) <$> lookup table pc

clearBreakpoint :: BreakpointTable -> Word16 -> IO ()
clearBreakpoint = delete

listBreakpoints :: BreakpointTable -> IO [(Word16, Bool)]
listBreakpoints = toList
