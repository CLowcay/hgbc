{-# LANGUAGE  OverloadedStrings #-}

module Debugger.LabelGenerator
  ( nextGlobalLabel
  , nextLocalLabel
  )
where

import           System.IO.Unsafe
import qualified Data.Text                     as T
import           Data.IORef

{-# NOINLINE globalCounter #-}
globalCounter :: IORef Int
globalCounter = unsafePerformIO (newIORef 0)

{-# NOINLINE localCounter #-}
localCounter :: IORef Int
localCounter = unsafePerformIO (newIORef 0)

formatNumber :: Int -> T.Text
formatNumber n = T.pack . reverse . take 6 $ reverse (show n) ++ repeat '0'

nextGlobalLabel :: IO T.Text
nextGlobalLabel = do
  i <- readIORef globalCounter
  writeIORef globalCounter $! i + 1
  pure ("proc" <> formatNumber i)

nextLocalLabel :: Integral a => a -> IO T.Text
nextLocalLabel v = do
  i <- readIORef localCounter
  writeIORef localCounter $! i + 1
  pure ((if v <= 0 then ".loop" else ".next") <> formatNumber i)
