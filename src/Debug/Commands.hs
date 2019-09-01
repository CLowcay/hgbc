{-# LANGUAGE LambdaCase #-}

module Debug.Commands where

import           Common
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor
import           Data.Word
import           Debug.Breakpoints
import           Debug.Dump
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GBC.CPU
import           GBC.Memory

-- | Debugger commands.
data Command = ShowHeader
             | ShowRegs
             | ShowMem Word16
             | ShowDisassembly (Maybe Word16)
             | Step Int
             | Run
             | Reset
             | AddBreakpoint Word16
             | DeleteBreakpoint Word16
             | DisableBreakpiont Word16
             | ListBreakpoints
             deriving (Eq, Ord, Show)

-- | The current state of the debugger.
data DebugState = DebugState {
    cpu :: !CPUState
  , breakpoints :: BreakpointTable
}

initDebug :: CPUState -> IO DebugState
initDebug cpuState = DebugState cpuState <$> initBreakpointTable

-- | The debugger monad.
type Debug a = ReaderT Memory (StateT DebugState IO) a

runDebug :: Memory -> DebugState -> Debug a -> IO a
runDebug mem debugState computation = evalStateT (runReaderT computation mem) debugState

hoistCPU :: CPU a -> Debug a
hoistCPU computation = do
  mem            <- ask
  debugState     <- get
  cpuState       <- gets cpu
  (r, cpuState') <- liftIO $ runStateT (runReaderT computation mem) cpuState
  put $ debugState { cpu = cpuState' }
  pure r

breakpointDecorator :: Debug (Word16 -> IO Char)
breakpointDecorator = do
  table <- gets breakpoints
  pure $ \addr -> getBreakpoint table addr <&> \case
    Nothing    -> ' '
    Just True  -> '*'
    Just False -> '-'

doCommand :: Command -> Debug ()
doCommand ShowHeader = do
  mem <- ask
  liftIO . dumpHeader $ getROMHeader mem
doCommand ShowRegs = do
  rawRegisters <- registers <$> gets cpu
  registerFile <- liftIO $ withForeignPtr rawRegisters peek
  liftIO $ dumpRegisters registerFile
doCommand (ShowMem addr) = do
  mem <- ask
  liftIO $ dumpMem mem addr
doCommand (ShowDisassembly Nothing) = do
  mem       <- ask
  pc        <- hoistCPU readPC
  decorator <- breakpointDecorator
  liftIO $ dumpDisassembly decorator mem pc 10
doCommand (ShowDisassembly (Just addr)) = do
  mem       <- ask
  decorator <- breakpointDecorator
  liftIO $ dumpDisassembly decorator mem addr 10
doCommand (Step n0) = do
  mem        <- ask
  breakpoint <- gets breakpoints
  hoistCPU $ go breakpoint n0
  decorator <- breakpointDecorator
  pc        <- hoistCPU readPC
  liftIO $ dumpDisassembly decorator mem pc 4
 where
  go _          0 = pure ()
  go breakpoint n = do
    void cpuStep
    brk <- shouldBreak breakpoint
    if brk then pure () else go breakpoint (n - 1)
doCommand Run = do
  mem        <- ask
  breakpoint <- gets breakpoints
  hoistCPU $ go breakpoint
  decorator <- breakpointDecorator
  pc        <- hoistCPU readPC
  liftIO $ dumpDisassembly decorator mem pc 4
 where
  go breakpoint = do
    void cpuStep
    brk <- shouldBreak breakpoint
    if brk then pure () else go breakpoint
doCommand Reset                = hoistCPU reset
doCommand (AddBreakpoint addr) = do
  table <- gets breakpoints
  liftIO $ setBreakpoint table addr True
doCommand (DeleteBreakpoint addr) = do
  table <- gets breakpoints
  liftIO $ clearBreakpoint table addr
doCommand (DisableBreakpiont addr) = do
  table <- gets breakpoints
  liftIO $ setBreakpoint table addr False
doCommand ListBreakpoints = do
  table <- gets breakpoints
  liftIO $ do
    allBreakpoints <- listBreakpoints table
    when (null allBreakpoints) $ putStrLn "No breakpoints set"
    forM_ allBreakpoints
      $ \(addr, enabled) -> putStrLn $ formatHex addr ++ if not enabled then " (disabled)" else ""
