module Main where

import           GBC.ROM
import           GBC.Memory
import           GBC.CPU
import           Debug.CLI
import           Debug.Commands
import           System.Environment
import           Control.Monad.Loops
import           Control.Monad.IO.Class
import           System.Exit
import qualified Data.ByteString               as B

main :: IO ()
main = do
  [romFile] <- getArgs
  romData   <- B.readFile romFile
  case validateROM romData of
    Left err -> do
      putStrLn $ "Error validating ROM " ++ show romFile ++ ": " ++ err
      exitFailure
    Right rom -> do
      let header = extractHeader rom
      mem       <- initMemory rom
      cpuState0 <- initCPU
      cpuState  <- runCPU mem cpuState0 $ do
        writePC $ startAddress header
        setIME
      runDebugger mem (DebugState cpuState)

runDebugger :: Memory -> DebugState -> IO ()
runDebugger mem debugState = runDebug mem debugState $ whileJust_ (liftIO nextCommand) doCommand
