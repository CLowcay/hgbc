module Main where

import           Control.Monad.Reader
import           Debug.CLI
import           Debug.Debugger
import           GBC.CPU
import           GBC.Emulator
import           GBC.Graphics.PPU
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
      emulatorState <- initEmulatorState rom
      debugState    <- initDebug rom emulatorState
      void $ startOutput (memory emulatorState) (vram emulatorState) (graphicsSync emulatorState) (mode emulatorState)
      runReaderT reset debugState
      runReaderT (Haskeline.runInputT Haskeline.defaultSettings cli) debugState
