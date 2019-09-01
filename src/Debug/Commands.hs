{-# LANGUAGE LambdaCase #-}

module Debug.Commands where

import           Common
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Loops
import           Data.Functor
import           Data.IORef
import           Data.Word
import           Debug.Breakpoints
import           Debug.Dump
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GBC.CPU
import           GBC.ISA
import           GBC.Memory

-- | Debugger commands.
data Command = ShowHeader
             | ShowRegs
             | ShowMem Word16
             | ShowDisassembly (Maybe Word16)
             | Step Int
             | Run
             | RunTo Word16
             | Reset
             | Poke8 Word16 Word8
             | PokeR8 Register8 Word8
             | PokeR16 Register16 Word16
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

-- | Initialise the debugger.
initDebug :: CPUState -> IO DebugState
initDebug cpuState = DebugState cpuState <$> initBreakpointTable

-- | The debugger monad.
type Debug a = ReaderT Memory (StateT DebugState IO) a

-- | Run the debugger.
runDebug :: Memory -> DebugState -> Debug a -> IO a
runDebug mem debugState computation = evalStateT (runReaderT computation mem) debugState

-- | Lift a CPU computation into the Debug monad.
hoistCPU :: CPU a -> Debug a
hoistCPU computation = do
  mem            <- ask
  debugState     <- get
  cpuState       <- gets cpu
  (r, cpuState') <- liftIO $ runStateT (runReaderT computation mem) cpuState
  put $ debugState { cpu = cpuState' }
  pure r

-- | Create a function to decorate disassembly dumps. Adds symbols to indicate
-- breakpoints, PC and other information.
makeDecorator :: Debug (Word16 -> IO String)
makeDecorator = do
  table <- gets breakpoints
  pc    <- hoistCPU readPC
  pure $ \addr -> do
    bp <- getBreakpoint table addr <&> \case
      Nothing    -> ' '
      Just True  -> '*'
      Just False -> '-'
    pure [bp, if addr == pc then '>' else ' ']

-- | A condition to check before executing the next instruction. Break if the
-- condition is True.
type BreakPreCondition = CPU Bool

-- | A condition to check after executing the next instruction. Break if the
-- condition is True.
type BreakPostCondition = DebugInfo -> CPU Bool

-- | Break after a certain number of instructions have been executed.
breakOnCountOf :: Int -> Debug BreakPreCondition
breakOnCountOf n0 = do
  counter <- liftIO $ newIORef n0
  pure . liftIO $ do
    c <- readIORef counter
    if c == 0
      then pure True
      else do
        writeIORef counter $c - 1
        pure False

-- | Break when breakpoints are triggered.
breakOnBreakpoints :: Debug BreakPostCondition
breakOnBreakpoints = do
  table <- gets breakpoints
  pure . const $ shouldBreak table

-- | Break when the PC equals a certain value.
breakOnPC :: Word16 -> BreakPostCondition
breakOnPC pc = const $ (pc ==) <$> readPC

-- | Run the interpreter until one of the break conditions is met.
doRun :: [BreakPreCondition] -> [BreakPostCondition] -> Debug ()
doRun preConditions postConditions = hoistCPU go
 where
  go = do
    doPreBreak <- orM preConditions
    if doPreBreak
      then pure ()
      else do
        debugInfo   <- cpuStep
        doPostBreak <- orM $ fmap ($debugInfo) postConditions
        if doPostBreak then pure () else go

-- | Disassemble the next 4 instructions at the current PC.
disassembleAtPC :: Debug ()
disassembleAtPC = do
  mem       <- ask
  decorator <- makeDecorator
  pc        <- hoistCPU readPC
  liftIO $ dumpDisassembly decorator mem pc 4

-- | Execute a debugger command.
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
  decorator <- makeDecorator
  liftIO $ dumpDisassembly decorator mem pc 10
doCommand (ShowDisassembly (Just addr)) = do
  mem       <- ask
  decorator <- makeDecorator
  liftIO $ dumpDisassembly decorator mem addr 10
doCommand (Step n) = do
  preConditions  <- sequence [breakOnCountOf n]
  postConditions <- sequence [breakOnBreakpoints]
  doRun preConditions postConditions
  disassembleAtPC
doCommand Run = do
  postConditions <- sequence [breakOnBreakpoints]
  doRun [] postConditions
  disassembleAtPC
doCommand (RunTo breakAddr) = do
  postConditions <- sequence [breakOnBreakpoints, pure $ breakOnPC breakAddr]
  doRun [] postConditions
  disassembleAtPC
doCommand Reset              = hoistCPU reset
doCommand (Poke8 addr value) = do
  mem <- ask
  liftIO $ writeMem mem addr value
doCommand (PokeR8  reg value ) = hoistCPU $ writeR8 reg value
doCommand (PokeR16 reg value ) = hoistCPU $ writeR16 reg value
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
