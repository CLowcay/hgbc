module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Debug.CLI
import           Debug.Commands
import           GBC.CPU
import           GBC.Memory
import           GBC.ROM
import           System.Environment
import           System.Exit
import qualified Data.ByteString               as B
import qualified SDL                           as SDL

main :: IO ()
main = do
  SDL.initializeAll
  [romFile] <- getArgs
  romData   <- B.readFile romFile
  case validateROM romData of
    Left err -> do
      putStrLn $ "Error validating ROM " ++ show romFile ++ ": " ++ err
      exitFailure
    Right rom -> do
      mem        <- initMemory rom
      cpuState0  <- initCPU
      cpuState   <- runCPU mem cpuState0 reset
      debugState <- initDebug romFile cpuState
      runDebugger mem debugState

runDebugger :: Memory -> DebugState -> IO ()
runDebugger mem debugState = runDebug mem debugState $ whileJust_ (liftIO nextCommand) doCommand
