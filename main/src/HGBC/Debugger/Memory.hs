{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.Memory
  ( backtrace,
    stackAt,
    memoryAt,
  )
where

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.String (IsString (fromString))
import Data.Traversable (for)
import Data.Word (Word16)
import HGBC.Debugger.JSON as JSON (longAddress)
import qualified Machine.GBC.CPU as CPU
import Machine.GBC.Disassembler (LongAddress (LongAddress))
import qualified Machine.GBC.Emulator as Emulator
import Machine.GBC.Memory (readChunk)
import Machine.GBC.Util (formatHex)

memoryChunkWidth :: Int
memoryChunkWidth = 8

backtrace :: Emulator.State -> IO LBC.ByteString
backtrace emulatorState =
  BB.toLazyByteString
    . JSON.fromEncoding
    . JSON.list (JSON.longAddress . uncurry LongAddress)
    <$> runReaderT CPU.getBacktrace emulatorState

stackAt :: Emulator.State -> Word16 -> Int -> IO LBC.ByteString
stackAt emulatorState offset n = do
  bytes <- fmap formatHex . B.unpack <$> runReaderT (readChunk offset (n + 1)) emulatorState
  pure
    ( LBC.intercalate "\n" . reverse $
        fromString
          <$> zipWith
            (\a b -> a <> " " <> b)
            bytes
            (tail bytes)
    )

memoryAt :: Emulator.State -> Word16 -> Int -> IO LBC.ByteString
memoryAt emulatorState address memLines = do
  chunks <-
    runReaderT
      ( for [0 .. (memLines - 1)] $
          \i -> readChunk (address + fromIntegral (memoryChunkWidth * i)) memoryChunkWidth
      )
      emulatorState
  pure (LBC.intercalate "\n" (formatChunk <$> chunks))
  where
    formatChunk s = LBC.intercalate " " $ LBC.pack . formatHex <$> B.unpack s
