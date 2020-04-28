module HGBC.Debugger.Breakpoints
  ( getAsList
  , set
  , disable
  , unset
  )
where

import           HGBC.Debugger.State
import           Machine.GBC.Disassembler
import qualified Data.HashTable.IO             as H
import qualified HGBC.Debugger.Events          as Event

getAsList :: DebugState -> IO [(LongAddress, Bool)]
getAsList debugState = H.toList (breakpoints debugState)

set :: DebugState -> Event.Channel -> LongAddress -> IO ()
set debugState channel address = do
  H.insert (breakpoints debugState) address True
  saveBreakpoints debugState
  Event.send channel (Event.BreakPointSet address)

disable :: DebugState -> Event.Channel -> LongAddress -> IO ()
disable debugState channel address = do
  H.insert (breakpoints debugState) address False
  saveBreakpoints debugState
  Event.send channel (Event.BreakPointDisabled address)

unset :: DebugState -> Event.Channel -> LongAddress -> IO ()
unset debugState channel address = do
  H.delete (breakpoints debugState) address
  saveBreakpoints debugState
  Event.send channel (Event.BreakPointRemoved address)
