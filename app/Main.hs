module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Debug.CLI
import           Debug.Commands
import           GBC.Bus
import           GBC.CPU
import           GBC.Memory
import           GBC.ROM
import           System.Environment
import           System.IO
import           System.Exit
import qualified Data.ByteString               as B
import qualified SDL

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
runDebugger mem debugState = do
  channel     <- newEmptyMVar
  commandDone <- newEmptyMVar
  hSetBuffering stdout NoBuffering
  let commandRunner = do
        mcommand <- liftIO (tryTakeMVar channel)
        case mcommand of
          Nothing -> do
            liftIO $ threadDelay 10000
            zoom bus handleEvents
          Just command -> do
            doCommand command
            liftIO $ do
              hFlush stdout
              putMVar commandDone ()
        commandRunner
  void . forkIO $ runDebug mem debugState commandRunner
  whileJust_ nextCommand $ \cmd -> putMVar channel cmd >> takeMVar commandDone

