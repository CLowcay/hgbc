module Debug.Commands where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Word
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
             | Reset
             deriving (Eq, Ord, Show)

-- | The current state of the debugger.
data DebugState = DebugState {
    cpu :: !CPUState
}

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
  mem <- ask
  pc  <- hoistCPU readPC
  liftIO $ dumpDisassembly mem pc 10
doCommand (ShowDisassembly (Just addr)) = do
  mem <- ask
  liftIO $ dumpDisassembly mem addr 10
doCommand (Step n0) = do
  mem <- ask
  hoistCPU $ go n0
  pc  <- hoistCPU readPC
  liftIO $ dumpDisassembly mem pc 4
 where
  go 0 = pure ()
  go n = do
    cpuStep
    go (n - 1)
doCommand Reset = hoistCPU $ writePC 0x150
