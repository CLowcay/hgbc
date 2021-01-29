{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.Labels
  ( addFromList,
    getAsList,
    update,
    delete,
  )
where

import Control.Exception (catch)
import Control.Monad (when)
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified Data.Text as T
import HGBC.Debugger.State (DebugState (labelsRef), saveLabels)
import qualified HGBC.Events as Event
import Machine.GBC.Disassembler (Labels, LongAddress)

-- | Add some new labels.
addFromList :: DebugState -> Event.Channel -> Labels -> IO ()
addFromList debugState channel newLabels = do
  modifyIORef' (labelsRef debugState) (`HM.union` HM.fromList newLabels)
  saveLabels debugState `catch` (Event.send channel . Event.IOWarning "Labels.addFromList")
  Event.send channel (Event.LabelUpdated newLabels)

-- | Get all of the labels as a list.
getAsList :: DebugState -> IO Labels
getAsList debugState = HM.toList <$> readIORef (labelsRef debugState)

-- | Update a label.
update :: DebugState -> Event.Channel -> LongAddress -> T.Text -> IO ()
update debugState channel address rawText =
  if T.null text
    then delete debugState channel address
    else do
      labels <- readIORef (labelsRef debugState)
      when (maybe True snd (HM.lookup address labels)) $ do
        writeIORef (labelsRef debugState) $! HM.insert address (text, True) labels
        saveLabels debugState `catch` (Event.send channel . Event.IOWarning "Labels.update")
        Event.send channel (Event.LabelUpdated [(address, (text, True))])
  where
    text = T.filter (not . isSpace) rawText

-- | Delete a label.
delete :: DebugState -> Event.Channel -> LongAddress -> IO ()
delete debugState channel address = do
  modifyIORef' (labelsRef debugState) (HM.delete address)
  saveLabels debugState `catch` (Event.send channel . Event.IOWarning "Labels.delete")
  Event.send channel (Event.LabelRemoved address)
