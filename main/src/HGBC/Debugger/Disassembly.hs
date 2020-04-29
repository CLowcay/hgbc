{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.Disassembly
  ( get
  , set
  , getAll
  , getN
  )
where

import           Data.IORef
import           Data.String
import           HGBC.Debugger.State
import           Machine.GBC.Disassembler
import qualified HGBC.Debugger.JSON            as JSON
import qualified Data.Aeson.Encoding           as JSON
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text.Lazy                as LT

get :: DebugState -> IO Disassembly
get debugState = readIORef (disassemblyRef debugState)

set :: DebugState -> Disassembly -> IO ()
set debugState disassembly = writeIORef (disassemblyRef debugState) $! disassembly

getAll :: DebugState -> IO LT.Text
getAll debugState = do
  disassembly <- readIORef (disassemblyRef debugState)
  labels      <- readIORef (labelsRef debugState)
  pure
    (";; " <> fromString (romFileName debugState) <> "\n\n" <> generateOutput
      disassembly
      ((fst <$>) . (`HM.lookup` labels))
    )

getN :: DebugState -> LongAddress -> Int -> IO LBC.ByteString
getN debugState address n = do
  disassembly <- readIORef (disassemblyRef debugState)
  pure . BB.toLazyByteString . JSON.fromEncoding . JSON.fields $ lookupN disassembly n address
