{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

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
  , symbolTable :: SymbolTable
  , breakpoints :: BreakpointTable
}

initSymbolTable :: SymbolTable
initSymbolTable =
  [ (0xFF00, "P1")
  , (0xFF01, "SB")
  , (0xFF01, "SB")
  , (0xFF02, "SC")
  , (0xFF04, "DIV")
  , (0xFF05, "TIMA")
  , (0xFF06, "TMA")
  , (0xFF07, "TAC")
  , (0xFF0F, "IF")
  , (0xFF10, "NR10")
  , (0xFF11, "NR11")
  , (0xFF12, "NR12")
  , (0xFF13, "NR13")
  , (0xFF14, "NR14")
  , (0xFF16, "NR21")
  , (0xFF17, "NR22")
  , (0xFF18, "NR23")
  , (0xFF19, "NR24")
  , (0xFF1A, "NR30")
  , (0xFF1B, "NR31")
  , (0xFF1C, "NR32")
  , (0xFF1D, "NR33")
  , (0xFF1E, "NR34")
  , (0xFF20, "NR41")
  , (0xFF21, "NR42")
  , (0xFF22, "NR43")
  , (0xFF23, "NR44")
  , (0xFF24, "NR50")
  , (0xFF25, "NR51")
  , (0xFF26, "NR52")
  , (0xFF40, "LCDC")
  , (0xFF41, "STAT")
  , (0xFF42, "SCY")
  , (0xFF43, "SCX")
  , (0xFF44, "LY")
  , (0xFF45, "LYC")
  , (0xFF46, "DMA")
  , (0xFF47, "BGP")
  , (0xFF48, "OBP0")
  , (0xFF49, "OBP1")
  , (0xFF4A, "WY")
  , (0xFF4B, "WX")
  , (0xFF4D, "KEY1")
  , (0xFF4F, "VBK")
  , (0xFF51, "HDMA1")
  , (0xFF52, "HDMA2")
  , (0xFF53, "HDMA3")
  , (0xFF54, "HDMA4")
  , (0xFF55, "HDMA5")
  , (0xFF56, "RP")
  , (0xFF68, "BCPS")
  , (0xFF69, "BCPD")
  , (0xFF6A, "OCPS")
  , (0xFF6B, "OCPD")
  , (0xFF70, "SVBK")
  , (0xFFFF, "IE")
  ]

-- | Initialise the debugger.
initDebug :: CPUState -> IO DebugState
initDebug cpuState = DebugState cpuState initSymbolTable <$> initBreakpointTable

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
  liftIO $ dumpDisassembly decorator symbolTable mem pc 4

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
  liftIO $ dumpMem mem addr
doCommand (ShowDisassembly Nothing) = do
  mem             <- ask
  DebugState {..} <- get
  pc              <- hoistCPU readPC
  decorator       <- makeDecorator
  liftIO $ dumpDisassembly decorator symbolTable mem pc 10
doCommand (ShowDisassembly (Just addr)) = do
  mem             <- ask
  DebugState {..} <- get
  decorator       <- makeDecorator
  liftIO $ dumpDisassembly decorator symbolTable mem addr 10
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
doCommand Reset                = hoistCPU reset
doCommand (Poke8   addr value) = writeMem addr value
doCommand (PokeR8  reg  value) = hoistCPU $ writeR8 reg value
doCommand (PokeR16 reg  value) = hoistCPU $ writeR16 reg value
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
