{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Debug.Commands
  ( Command(..)
  , MemAddress(..)
  , DebugState
  , bus
  , initDebug
  , doCommand
  )
where

import           Common
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Word
import           Debug.Breakpoints
import           Debug.Dump
import           Debug.Map
import           GBC.Bus
import           GBC.CPU
import           GBC.Graphics
import           GBC.Decode
import           GBC.ISA
import           GBC.Memory

{-# SPECIALIZE readPC :: ReaderT DebugState IO Word16 #-}
{-# SPECIALIZE busStep :: ReaderT DebugState IO (BusEvent, Maybe Update) #-}

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
    bus :: !BusState
  , romFile :: !FilePath
  , codeMap :: !(IORef SymbolTable)
  , breakpoints :: !BreakpointTable
}

instance HasBusState DebugState where
  {-# INLINE forBusState #-}
  forBusState = bus

instance HasDebugState DebugState where
  {-# INLINE forDebugState #-}
  forDebugState = id

class HasDebugState env where
  forDebugState :: env -> DebugState

type UsesDebug env m = (UsesBus env m, HasDebugState env)

-- | Initialise the debugger.
{-# INLINABLE initDebug #-}
initDebug :: FilePath -> CPUState -> Memory -> IO DebugState
initDebug thisROMFile cpuState mem = do
  busState <- initBusState cpuState mem
  DebugState busState thisROMFile <$> (newIORef =<< initMap thisROMFile) <*> initBreakpointTable

{-# INLINABLE getCodeMap #-}
getCodeMap :: UsesDebug env m => ReaderT env m SymbolTable
getCodeMap = liftIO . readIORef =<< codeMap <$> asks forDebugState

{-# INLINABLE setCodeMap #-}
setCodeMap :: UsesDebug env m => SymbolTable -> ReaderT env m ()
setCodeMap value = liftIO . (`writeIORef` value) =<< codeMap <$> asks forDebugState

-- | Create a function to decorate disassembly dumps. Adds symbols to indicate
-- breakpoints, PC and other information.
{-# INLINABLE makeDecorator #-}
makeDecorator :: UsesDebug env m => ReaderT env m (Word16 -> IO String)
makeDecorator = do
  DebugState {..} <- asks forDebugState
  pc              <- readPC
  pure $ \addr -> do
    bp <- getBreakpoint breakpoints addr <&> \case
      Nothing    -> ' '
      Just True  -> '*'
      Just False -> '-'
    pure [bp, if addr == pc then '>' else ' ']

-- | A condition to check before executing the next instruction. Break if the
-- condition is True.
type BreakPreCondition env m = ReaderT env m Bool

-- | A condition to check after executing the next instruction. Break if the
-- condition is True.
type BreakPostCondition env m = BusEvent -> ReaderT env m Bool

-- | Break after a certain number of instructions have been executed.
{-# INLINABLE breakOnCountOf #-}
breakOnCountOf :: MonadIO m => Int -> ReaderT env m (BreakPreCondition env m)
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
{-# INLINABLE breakOnBreakpoints #-}
breakOnBreakpoints :: UsesDebug env m => ReaderT env m (BreakPostCondition env m)
breakOnBreakpoints = do
  DebugState {..} <- asks forDebugState
  pure . const $ shouldBreak breakpoints

-- | Break when the PC equals a certain value.
{-# INLINABLE breakOnPC #-}
breakOnPC :: UsesCPU env m => Word16 -> BreakPostCondition env m
breakOnPC pc = const $ (pc ==) <$> readPC

-- | Break when a RET instruction is executed with the current stack pointer.
{-# INLINABLE breakOnRet #-}
breakOnRet :: UsesCPU env m => Word16 -> BreakPreCondition env m
breakOnRet originalSP = do
  sp          <- readR16 RegSP
  instruction <- decodeOnly decode
  case instruction of
    RET               -> pure $ originalSP == sp
    (RETCC condition) -> do
      shouldRet <- testCondition condition
      pure $ shouldRet && originalSP == sp
    RETI -> pure $ originalSP == sp
    _    -> pure False

-- | Run the interpreter until one of the break conditions is met.
{-# INLINABLE doRun #-}
doRun
  :: UsesBus env m => [BreakPreCondition env m] -> [BreakPostCondition env m] -> ReaderT env m ()
doRun preConditions postConditions = clearBreakFlag >> go
 where
  go = do
    doPreBreak <- orM preConditions
    if doPreBreak
      then pure ()
      else do
        (debugInfo, _) <- busStep
        breakFlag      <- isBreakFlagSet
        doPostBreak    <- orM $ fmap ($ debugInfo) postConditions
        if doPostBreak || breakFlag then pure () else go

-- | Disassemble the next 4 instructions at the current PC.
{-# INLINABLE disassembleAtPC #-}
disassembleAtPC :: UsesDebug env m => ReaderT env m ()
disassembleAtPC = do
  symbols   <- getCodeMap
  decorator <- makeDecorator
  pc        <- readPC
  dumpDisassembly decorator symbols pc 4

{-# INLINABLE withAddress #-}
withAddress :: UsesDebug env m => MemAddress -> (Word16 -> ReaderT env m ()) -> ReaderT env m ()
withAddress (ConstAddress addr ) action = action addr
withAddress (LabelAddress label) action = do
  symbols <- getCodeMap
  case lookupBySymbol symbols label of
    Just addr -> action addr
    Nothing   -> liftIO $ putStrLn $ "Unknown symbol " ++ label

-- | Execute a debugger command.
{-# SPECIALIZE doCommand :: Command -> ReaderT DebugState IO () #-}
doCommand :: UsesDebug env m => Command -> ReaderT env m ()
doCommand ShowHeader = do
  mem <- asks forMemory
  liftIO . dumpHeader $ getROMHeader mem
doCommand ShowRegs = do
  registerFile <- getRegisterFile
  liftIO $ dumpRegisters registerFile
doCommand (ShowMem         addr   ) = withAddress addr dumpMem
doCommand (ShowDisassembly Nothing) = do
  pc        <- readPC
  decorator <- makeDecorator
  symbols   <- getCodeMap
  dumpDisassembly decorator symbols pc 10
doCommand (ShowDisassembly (Just addr)) = do
  decorator <- makeDecorator
  symbols   <- getCodeMap
  withAddress addr $ \actualAddress -> dumpDisassembly decorator symbols actualAddress 10
doCommand (Step n) = do
  preConditions  <- sequence [breakOnCountOf n]
  postConditions <- sequence [breakOnBreakpoints]
  doRun preConditions postConditions
  disassembleAtPC
doCommand StepOut = do
  sp             <- readR16 RegSP
  postConditions <- sequence [breakOnBreakpoints]
  doRun [breakOnRet sp] postConditions
  DebugState {..} <- asks forDebugState
  wasBreakpoint   <- shouldBreak breakpoints
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
doCommand Reset                = reset
doCommand (Poke8   addr value) = withAddress addr $ \actualAddr -> writeByte actualAddr value
doCommand (PokeR8  reg  value) = writeR8 reg value
doCommand (PokeR16 reg  value) = writeR16 reg value
doCommand (AddBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- asks forDebugState
  liftIO $ setBreakpoint breakpoints breakAddr True
doCommand (DeleteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- asks forDebugState
  liftIO $ clearBreakpoint breakpoints breakAddr
doCommand (DisableBreakpiont addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- asks forDebugState
  liftIO $ setBreakpoint breakpoints breakAddr False
doCommand ListBreakpoints = do
  DebugState {..} <- asks forDebugState
  liftIO $ do
    allBreakpoints <- listBreakpoints breakpoints
    when (null allBreakpoints) $ putStrLn "No breakpoints set"
    for_ allBreakpoints
      $ \(addr, enabled) -> putStrLn $ formatHex addr ++ if not enabled then " (disabled)" else ""
doCommand (AddSymbol symbol value) = do
  DebugState {..} <- asks forDebugState
  codeMap'        <- addToMap (symbol, value) <$> getCodeMap
  liftIO $ saveMap (mapFileName romFile) codeMap'
  setCodeMap codeMap'
doCommand (DeleteSymbol symbol) = do
  DebugState {..} <- asks forDebugState
  codeMap'        <- removeFromMap symbol <$> getCodeMap
  liftIO $ saveMap (mapFileName romFile) codeMap'
  setCodeMap codeMap'
doCommand ListSymbols = do
  symbols <- listSymbols <$> getCodeMap
  for_ symbols $ \(label, value) -> liftIO $ putStrLn $ label ++ ": " ++ formatHex value
