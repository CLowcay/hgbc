module Main where

import           Control.Monad.Reader
import           Debug.CLI
import           Debug.Debugger
import           GBC.CPU
import           GBC.Graphics
import           GBC.GraphicsSync
import           GBC.Memory
import           GBC.ROM
import           System.Environment
import           System.Exit
import qualified Data.ByteString               as B
import qualified SDL
import qualified System.Console.Haskeline      as Haskeline

main :: IO ()
main = do
  SDL.initializeAll
  [file]  <- getArgs
  romData <- B.readFile file
  case validateROM file romData of
    Left err -> do
      putStrLn $ "Error validating ROM " ++ show file ++ ": " ++ err
      exitFailure
    Right rom -> do
      sync              <- newGraphicsSync
      (videoBuffers, _) <- startOutput sync
      mem               <- initMemory rom videoBuffers
      cpuState          <- initCPU
      debugState        <- initDebug file cpuState mem sync
      runReaderT reset debugState
      runReaderT (Haskeline.runInputT Haskeline.defaultSettings cli) debugState
