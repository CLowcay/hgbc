module HGBC.Debugger.Breakpoints
  ( getAsList,
    check,
    set,
    disable,
    unset,
  )
where

import Control.Exception (catch)
import qualified Data.HashTable.IO as H
import Data.Maybe
import HGBC.Debugger.State
import qualified HGBC.Events as Event
import Machine.GBC.Disassembler

getAsList :: DebugState -> IO [(LongAddress, Bool)]
getAsList debugState = H.toList (breakpoints debugState)

check :: DebugState -> LongAddress -> IO Bool
check debugState address = fromMaybe False <$> H.lookup (breakpoints debugState) address

set :: DebugState -> Event.Channel -> LongAddress -> IO ()
set debugState channel address = do
  H.insert (breakpoints debugState) address True
  saveBreakpoints debugState `catch` (Event.send channel . Event.IOWarning "Breakpoints.set")
  Event.send channel (Event.BreakPointSet address)

disable :: DebugState -> Event.Channel -> LongAddress -> IO ()
disable debugState channel address = do
  H.insert (breakpoints debugState) address False
  saveBreakpoints debugState `catch` (Event.send channel . Event.IOWarning "Breakpoints.disable")
  Event.send channel (Event.BreakPointDisabled address)

unset :: DebugState -> Event.Channel -> LongAddress -> IO ()
unset debugState channel address = do
  H.delete (breakpoints debugState) address
  saveBreakpoints debugState `catch` (Event.send channel . Event.IOWarning "Breakpoints.unset")
  Event.send channel (Event.BreakPointRemoved address)
