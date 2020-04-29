{-# LANGUAGE OverloadedStrings #-}
module HGBC.Debugger.Memory
  ( backtrace
  , stackAt
  , memoryAt
  )
where

import           Control.Monad.Reader
import           Data.String
import           Data.Traversable
import           Data.Word
import           HGBC.Debugger.JSON            as JSON
import           Machine.GBC.Disassembler
import           Machine.GBC.Memory             ( readChunk )
import           Machine.GBC.Util               ( formatHex )
import qualified Data.Aeson.Encoding           as JSON
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Machine.GBC                   as GBC

memoryChunkWidth :: Int
memoryChunkWidth = 8

backtrace :: GBC.EmulatorState -> IO LBC.ByteString
backtrace emulatorState =
  BB.toLazyByteString
    .   JSON.fromEncoding
    .   JSON.list (JSON.longAddress . uncurry LongAddress)
    <$> runReaderT GBC.getCPUBacktrace emulatorState

stackAt :: GBC.EmulatorState -> Word16 -> Int -> IO LBC.ByteString
stackAt emulatorState offset n = do
  bytes <- fmap formatHex . B.unpack <$> runReaderT (readChunk offset (n + 1)) emulatorState
  pure
    (LBC.intercalate "\n" . reverse $ fromString <$> zipWith (\a b -> a <> " " <> b)
                                                             bytes
                                                             (tail bytes)
    )

memoryAt :: GBC.EmulatorState -> Word16 -> Int -> IO LBC.ByteString
memoryAt emulatorState address memLines = do
  chunks <- runReaderT
    ( for [0 .. (memLines - 1)]
    $ \i -> readChunk (address + fromIntegral (memoryChunkWidth * i)) memoryChunkWidth
    )
    emulatorState
  pure (LBC.intercalate "\n" (formatChunk <$> chunks))
  where formatChunk s = LBC.intercalate " " $ LBC.pack . formatHex <$> B.unpack s
