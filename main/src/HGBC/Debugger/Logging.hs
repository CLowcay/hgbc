module HGBC.Debugger.Logging
  ( logError
  )
where

import           Data.Time.Format
import           Data.Time.LocalTime
import           System.IO
import qualified Data.ByteString.Char8         as CB
import qualified Network.Wai                   as Wai

logError :: Wai.Request -> String -> IO ()
logError req message = do
  time <- getZonedTime
  hPutStrLn
    stderr
    (  "["
    <> formatTime defaultTimeLocale "%F %T%3Q UTC%z" time
    <> "] "
    <> (CB.unpack (Wai.requestMethod req) <> " ")
    <> (CB.unpack (Wai.rawPathInfo req) <> ": ")
    <> message
    )
