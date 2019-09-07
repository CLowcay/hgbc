{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Debug.Commands
  ( Command(..)
  , MemAddress(..)
  , DebugState
  , initDebug
  , runDebug
  , doCommand
  )
where

import           Common
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor
import           Data.IORef
import           Data.Word
import           Debug.Breakpoints
import           Debug.Dump
import           Debug.Map
import           GBC.CPU
import           GBC.ISA
import           GBC.Decode
import           GBC.Memory

data MemAddress = ConstAddress Word16 | LabelAddress String deriving (Eq, Ord, Show)

-- | Debugger commands.
data Command = ShowHeader
             | ShowRegs
             | ShowMem MemAddress
             | ShowDisassembly (Maybe MemAddress)
             | Step Int
             | StepOut
             | Run
             | RunTo MemAddress
             | Reset
             | Poke8 MemAddress Word8
             | PokeR8 Register8 Word8
             | PokeR16 Register16 Word16
             | AddBreakpoint MemAddress
             | DeleteBreakpoint MemAddress
             | DisableBreakpiont MemAddress
             | ListBreakpoints
             | AddSymbol String Word16
             | DeleteSymbol String
             | ListSymbols
             deriving (Eq, Ord, Show)

-- | The current state of the debugger.
data DebugState = DebugState {
    cpu :: !CPUState
  , romFile :: !FilePath
  , codeMap :: !SymbolTable
  , breakpoints :: !BreakpointTable
}

-- | Initialise the debugger.
initDebug :: FilePath -> CPUState -> IO DebugState
initDebug romFile cpuState = do
  codeMap <- initMap romFile
  DebugState cpuState romFile codeMap <$> initBreakpointTable

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
type BreakPostCondition = BusEvent -> CPU Bool

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

-- | Break when a RET instruction is executed with the current stack pointer.
breakOnRet :: Word16 -> BreakPreCondition
breakOnRet originalSP = do
  sp <- readR16 RegSP
  instruction <- decodeOnly decode
  case instruction of
    Just (RET condition) -> do
      shouldRet <- testCondition condition
      pure $ shouldRet && originalSP == sp
    Just RETI -> pure $ originalSP == sp
    _ -> pure False

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
  mem             <- ask
  DebugState {..} <- get
  decorator       <- makeDecorator
  pc              <- hoistCPU readPC
  liftIO $ dumpDisassembly decorator codeMap mem pc 4

withAddress :: MemAddress -> (Word16 -> Debug ()) -> Debug ()
withAddress (ConstAddress addr ) action = action addr
withAddress (LabelAddress label) action = do
  symbolTable <- gets codeMap
  case lookupBySymbol symbolTable label of
    Just addr -> action addr
    Nothing   -> liftIO $ putStrLn $ "Unknown symbol " ++ label

-- | Execute a debugger command.
doCommand :: Command -> Debug ()
doCommand ShowHeader = do
  mem <- ask
  liftIO . dumpHeader $ getROMHeader mem
doCommand ShowRegs = do
  registerFile <- hoistCPU getRegisterFile
  liftIO $ dumpRegisters registerFile
doCommand (ShowMem addr) = do
  mem <- ask
  withAddress addr $ liftIO . dumpMem mem
doCommand (ShowDisassembly Nothing) = do
  mem             <- ask
  DebugState {..} <- get
  pc              <- hoistCPU readPC
  decorator       <- makeDecorator
  liftIO $ dumpDisassembly decorator codeMap mem pc 10
doCommand (ShowDisassembly (Just addr)) = do
  mem             <- ask
  DebugState {..} <- get
  decorator       <- makeDecorator
  withAddress addr
    $ \actualAddress -> liftIO $ dumpDisassembly decorator codeMap mem actualAddress 10
doCommand (Step n) = do
  preConditions  <- sequence [breakOnCountOf n]
  postConditions <- sequence [breakOnBreakpoints]
  doRun preConditions postConditions
  disassembleAtPC
doCommand StepOut = do
  sp <- hoistCPU $ readR16 RegSP
  postConditions <- sequence [breakOnBreakpoints]
  doRun [breakOnRet sp] postConditions
  table <- gets breakpoints
  hoistCPU $ do
    wasBreakpoint <- shouldBreak table
    unless wasBreakpoint $ void cpuStep
  disassembleAtPC
doCommand Run = do
  postConditions <- sequence [breakOnBreakpoints]
  doRun [] postConditions
  disassembleAtPC
doCommand (RunTo breakAddr) = withAddress breakAddr $ \addr -> do
  postConditions <- sequence [breakOnBreakpoints, pure $ breakOnPC addr]
  doRun [] postConditions
  disassembleAtPC
doCommand Reset                = hoistCPU reset
doCommand (Poke8   addr value) = withAddress addr $ \actualAddr -> writeMem actualAddr value
doCommand (PokeR8  reg  value) = hoistCPU $ writeR8 reg value
doCommand (PokeR16 reg  value) = hoistCPU $ writeR16 reg value
doCommand (AddBreakpoint addr) = withAddress addr $ \breakAddr -> do
  table <- gets breakpoints
  liftIO $ setBreakpoint table breakAddr True
doCommand (DeleteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  table <- gets breakpoints
  liftIO $ clearBreakpoint table breakAddr
doCommand (DisableBreakpiont addr) = withAddress addr $ \breakAddr -> do
  table <- gets breakpoints
  liftIO $ setBreakpoint table breakAddr False
doCommand ListBreakpoints = do
  table <- gets breakpoints
  liftIO $ do
    allBreakpoints <- listBreakpoints table
    when (null allBreakpoints) $ putStrLn "No breakpoints set"
    forM_ allBreakpoints
      $ \(addr, enabled) -> putStrLn $ formatHex addr ++ if not enabled then " (disabled)" else ""
doCommand (AddSymbol symbol value) = do
  debug <- get
  let codeMap' = addToMap (codeMap debug) (symbol, value)
  modify $ \s -> s { codeMap = codeMap' }
  liftIO $ saveMap (mapFileName $ romFile debug) codeMap'
doCommand (DeleteSymbol symbol) = do
  debug <- get
  let codeMap' = removeFromMap (codeMap debug) symbol
  modify $ \s -> s { codeMap = codeMap' }
  liftIO $ saveMap (mapFileName $ romFile debug) codeMap'
doCommand ListSymbols = do
  symbols <- listSymbols <$> gets codeMap
  forM_ symbols $ \(label, value) -> liftIO $ putStrLn $ label ++ ": " ++ formatHex value
