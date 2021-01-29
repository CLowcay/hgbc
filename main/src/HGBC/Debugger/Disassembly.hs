{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.Disassembly
  ( get,
    set,
    getAll,
    getN,
  )
where

import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef)
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as LT
import qualified HGBC.Debugger.JSON as JSON
import HGBC.Debugger.State (DebugState (disassemblyRef, labelsRef, romFileName))
import Machine.GBC.Disassembler (Disassembly, LongAddress, generateOutput, lookupN)

get :: DebugState -> IO Disassembly
get debugState = readIORef (disassemblyRef debugState)

set :: DebugState -> Disassembly -> IO ()
set debugState disassembly = writeIORef (disassemblyRef debugState) $! disassembly

getAll :: DebugState -> IO LT.Text
getAll debugState = do
  disassembly <- readIORef (disassemblyRef debugState)
  labels <- readIORef (labelsRef debugState)
  pure
    ( ";; " <> fromString (romFileName debugState) <> "\n\n"
        <> generateOutput
          disassembly
          ((fst <$>) . (`HM.lookup` labels))
    )

getN :: DebugState -> LongAddress -> Int -> IO LBC.ByteString
getN debugState address n = do
  disassembly <- readIORef (disassemblyRef debugState)
  pure . BB.toLazyByteString . JSON.fromEncoding . JSON.fields $ lookupN disassembly n address
