module HGBC.Debugger.Logging
  ( logError,
  )
where

import qualified Data.ByteString.Char8 as CB
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Network.Wai as Wai
import System.IO (hPutStrLn, stderr)

logError :: Wai.Request -> String -> IO ()
logError req message = do
  time <- getZonedTime
  hPutStrLn
    stderr
    ( "["
        <> formatTime defaultTimeLocale "%F %T%3Q UTC%z" time
        <> "] "
        <> (CB.unpack (Wai.requestMethod req) <> " ")
        <> (CB.unpack (Wai.rawPathInfo req) <> ": ")
        <> message
    )
