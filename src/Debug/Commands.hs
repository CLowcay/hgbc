{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Control.Lens
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.IORef
import           Data.Word
import           Data.Foldable
import           Debug.Breakpoints
import           Debug.Dump
import           Debug.Map
import           Debug.TileViewer
import           GBC.Bus
import           GBC.Bus.Synchronizer
import           GBC.CPU
import           GBC.Decode
import           GBC.Graphics
import           GBC.ISA
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
             | ViewTiles
             deriving (Eq, Ord, Show)

-- | The current state of the debugger.
data DebugState = DebugState {
    _bus :: !BusState
  , _romFile :: !FilePath
  , _codeMap :: !SymbolTable
  , _breakpoints :: !BreakpointTable
}

makeLenses ''DebugState

-- | Initialise the debugger.
initDebug :: FilePath -> CPUState -> IO DebugState
initDebug thisROMFile cpuState = do
  thisCodeMap <- initMap thisROMFile
  DebugState (BusState cpuState initGraphics []) thisROMFile thisCodeMap <$> initBreakpointTable

-- | The debugger monad.
type Debug a = ReaderT Memory (StateT DebugState IO) a

-- | Run the debugger.
runDebug :: Memory -> DebugState -> Debug a -> IO a
runDebug mem debugState computation = evalStateT (runReaderT computation mem) debugState

-- | Create a function to decorate disassembly dumps. Adds symbols to indicate
-- breakpoints, PC and other information.
makeDecorator :: Debug (Word16 -> IO String)
makeDecorator = do
  table <- use breakpoints
  pc    <- zoom (bus . cpu) readPC
  pure $ \addr -> do
    bp <- getBreakpoint table addr <&> \case
      Nothing    -> ' '
      Just True  -> '*'
      Just False -> '-'
    pure [bp, if addr == pc then '>' else ' ']

-- | A condition to check before executing the next instruction. Break if the
-- condition is True.
type BreakPreCondition = Bus Bool

-- | A condition to check after executing the next instruction. Break if the
-- condition is True.
type BreakPostCondition = BusEvent -> Bus Bool

-- | Break after a certain number of instructions have been executed.
breakOnCountOf :: Int -> Debug BreakPreCondition
breakOnCountOf n0 = do
  counter <- liftIO $ newIORef n0
  pure . liftIO $ do
    c <- readIORef counter
    if c == 0
      then pure True
      else do
        writeIORef counter $ c - 1
        pure False

-- | Break when breakpoints are triggered.
breakOnBreakpoints :: Debug BreakPostCondition
breakOnBreakpoints = do
  table <- use breakpoints
  pure . const $ shouldBreak table

-- | Break when the PC equals a certain value.
breakOnPC :: Word16 -> BreakPostCondition
breakOnPC pc = const $ zoom cpu $ (pc ==) <$> readPC

-- | Break when a RET instruction is executed with the current stack pointer.
breakOnRet :: Word16 -> BreakPreCondition
breakOnRet originalSP = zoom cpu $ do
  sp          <- readR16 RegSP
  instruction <- decodeOnly decode
  case instruction of
    Just (RET condition) -> do
      shouldRet <- testCondition condition
      pure $ shouldRet && originalSP == sp
    Just RETI -> pure $ originalSP == sp
    _         -> pure False

-- | Run the interpreter until one of the break conditions is met.
doRun :: [BreakPreCondition] -> [BreakPostCondition] -> Debug ()
doRun preConditions postConditions = zoom bus go
 where
  go = do
    doPreBreak <- orM preConditions
    if doPreBreak
      then pure ()
      else do
        (debugInfo, _) <- busStep
        doPostBreak    <- orM $ fmap ($ debugInfo) postConditions
        if doPostBreak then pure () else go

-- | Disassemble the next 4 instructions at the current PC.
disassembleAtPC :: Debug ()
disassembleAtPC = do
  mem        <- ask
  debugState <- get
  decorator  <- makeDecorator
  pc         <- zoom (bus . cpu) readPC
  liftIO $ dumpDisassembly decorator (debugState ^. codeMap) mem pc 4

withAddress :: MemAddress -> (Word16 -> Debug ()) -> Debug ()
withAddress (ConstAddress addr ) action = action addr
withAddress (LabelAddress label) action = do
  symbolTable <- use codeMap
  case lookupBySymbol symbolTable label of
    Just addr -> action addr
    Nothing   -> liftIO $ putStrLn $ "Unknown symbol " ++ label

-- | Execute a debugger command.
doCommand :: Command -> Debug ()
doCommand ShowHeader = do
  mem <- ask
  liftIO . dumpHeader $ getROMHeader mem
doCommand ShowRegs = do
  registerFile <- zoom (bus . cpu) getRegisterFile
  liftIO $ dumpRegisters registerFile
doCommand (ShowMem addr) = do
  mem <- ask
  withAddress addr $ liftIO . dumpMem mem
doCommand (ShowDisassembly Nothing) = do
  mem       <- ask
  debug     <- get
  pc        <- zoom (bus . cpu) readPC
  decorator <- makeDecorator
  liftIO $ dumpDisassembly decorator (debug ^. codeMap) mem pc 10
doCommand (ShowDisassembly (Just addr)) = do
  mem       <- ask
  debug     <- get
  decorator <- makeDecorator
  withAddress addr
    $ \actualAddress -> liftIO $ dumpDisassembly decorator (debug ^. codeMap) mem actualAddress 10
doCommand (Step n) = do
  preConditions  <- sequence [breakOnCountOf n]
  postConditions <- sequence [breakOnBreakpoints]
  doRun preConditions postConditions
  disassembleAtPC
doCommand StepOut = do
  sp             <- zoom (bus . cpu) $ readR16 RegSP
  postConditions <- sequence [breakOnBreakpoints]
  doRun [breakOnRet sp] postConditions
  table <- use breakpoints
  zoom bus $ do
    wasBreakpoint <- shouldBreak table
    unless wasBreakpoint $ void busStep
  disassembleAtPC
doCommand Run = do
  postConditions <- sequence [breakOnBreakpoints]
  doRun [] postConditions
  disassembleAtPC
doCommand (RunTo breakAddr) = withAddress breakAddr $ \addr -> do
  postConditions <- sequence [breakOnBreakpoints, pure $ breakOnPC addr]
  doRun [] postConditions
  disassembleAtPC
doCommand Reset                = zoom (bus . cpu) reset
doCommand (Poke8   addr value) = withAddress addr $ \actualAddr -> writeMem actualAddr value
doCommand (PokeR8  reg  value) = zoom (bus . cpu) $ writeR8 reg value
doCommand (PokeR16 reg  value) = zoom (bus . cpu) $ writeR16 reg value
doCommand (AddBreakpoint addr) = withAddress addr $ \breakAddr -> do
  table <- use breakpoints
  liftIO $ setBreakpoint table breakAddr True
doCommand (DeleteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  table <- use breakpoints
  liftIO $ clearBreakpoint table breakAddr
doCommand (DisableBreakpiont addr) = withAddress addr $ \breakAddr -> do
  table <- use breakpoints
  liftIO $ setBreakpoint table breakAddr False
doCommand ListBreakpoints = do
  table <- use breakpoints
  liftIO $ do
    allBreakpoints <- listBreakpoints table
    when (null allBreakpoints) $ putStrLn "No breakpoints set"
    for_ allBreakpoints
      $ \(addr, enabled) -> putStrLn $ formatHex addr ++ if not enabled then " (disabled)" else ""
doCommand (AddSymbol symbol value) = do
  debug <- get
  let codeMap' = addToMap (debug ^. codeMap) (symbol, value)
  codeMap .= codeMap'
  liftIO $ saveMap (mapFileName $ debug ^. romFile) codeMap'
doCommand (DeleteSymbol symbol) = do
  debug <- get
  let codeMap' = removeFromMap (debug ^. codeMap) symbol
  codeMap .= codeMap'
  liftIO $ saveMap (mapFileName $ debug ^. romFile) codeMap'
doCommand ListSymbols = do
  symbols <- listSymbols <$> use codeMap
  for_ symbols $ \(label, value) -> liftIO $ putStrLn $ label ++ ": " ++ formatHex value
doCommand ViewTiles = do
  synchronizer <- liftIO newSynchronizer
  zoom bus (registerGraphicsSynchronizer synchronizer)
  mem <- ask
  liftIO $ startTileViewer synchronizer mem
