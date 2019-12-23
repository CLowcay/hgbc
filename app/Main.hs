module Main where

import           Control.Monad.Reader
import           Debug.CLI
import           Debug.Debugger
import           GBC.CPU
import           GBC.Emulator
import           GBC.Graphics
import           GBC.Graphics.Window
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
      sync                  <- newGraphicsSync
      (_, frameBufferBytes) <- startOutput sync
      emulatorState         <- initEmulatorState rom sync frameBufferBytes
      debugState            <- initDebug rom emulatorState
      runReaderT reset debugState
      runReaderT (Haskeline.runInputT Haskeline.defaultSettings cli) debugState
