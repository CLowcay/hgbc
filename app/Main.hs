module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Debug.CLI
import           Debug.Commands
import           GBC.Bus
import           GBC.CPU
import           GBC.Graphics
import           GBC.GraphicsOutput
import           GBC.Memory
import           GBC.ROM
import           System.Environment
import           System.Exit
import           System.IO
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
      sync              <- newGraphicsSync
      (videoBuffers, _) <- startOutput sync
      mem               <- initMemory rom videoBuffers
      cpuState          <- initCPU
      debugState        <- initDebug romFile cpuState mem sync
      runDebugger debugState

runDebugger :: DebugState -> IO ()
runDebugger debugState = do
  channel     <- newEmptyMVar
  commandDone <- newEmptyMVar
  hSetBuffering stdout NoBuffering
  let commandRunner = do
        mcommand <- liftIO (tryTakeMVar channel)
        case mcommand of
          Nothing -> do
            liftIO $ threadDelay 10000
            handleEvents
          Just command -> do
            doCommand command
            liftIO $ do
              hFlush stdout
              putMVar commandDone ()
        commandRunner
  void (forkIO (runReaderT commandRunner debugState))
  whileJust_ nextCommand $ \cmd -> putMVar channel cmd >> takeMVar commandDone
