{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

module Debug.Debugger
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
import           Data.Bits
import           Debug.Breakpoints
import           Debug.Dump
import           Debug.Map
import           GBC.Bus
import           GBC.Keypad
import           GBC.Timer
import           GBC.CPU
import           GBC.Decode
import           GBC.Graphics
import           GBC.ISA
import           GBC.Memory
import           Numeric
import qualified SDL.Time                      as SDL

{-# SPECIALIZE readPC :: ReaderT DebugState IO Word16 #-}
{-# SPECIALIZE busStep :: ReaderT DebugState IO BusEvent #-}

data MemAddress = ConstAddress !Word16 | LabelAddress !String !(Maybe Word16) deriving (Eq, Ord, Show)

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
             | Poke16 MemAddress Word16
             | Peek8 MemAddress
             | Peek16 MemAddress
             | PokeR8 Register8 Word8
             | PokeR16 Register16 Word16
             | ShowGraphics (Maybe String)
             | ShowTimer (Maybe String)
             | ShowAudio (Maybe String)
             | ShowInternal (Maybe String)
             | ShowMBC (Maybe String)
             | AddBreakpoint MemAddress
             | DeleteBreakpoint MemAddress
             | DisableBreakpiont MemAddress
             | AddWriteBreakpoint MemAddress
             | DeleteWriteBreakpoint MemAddress
             | DisableWriteBreakpiont MemAddress
             | ListBreakpoints
             | AddSymbol String Word16
             | DeleteSymbol String
             | ListSymbols
             deriving (Eq, Ord, Show)

-- | The current state of the debugger.
data DebugState = DebugState {
    bus :: !BusState
  , memory :: !Memory
  , cpu :: !CPUState
  , graphicsState :: !GraphicsState
  , graphicsSync :: !GraphicsSync
  , lcdEnabled :: !(IORef Bool)
  , keypadState :: !KeypadState
  , timerState :: !TimerState
  , romFile :: !FilePath
  , codeMap :: !(IORef SymbolTable)
  , executeBreakpoints :: !BreakpointTable
  , writeBreakpoints :: !BreakpointTable
}

instance HasMemory DebugState where
  {-# INLINE forMemory #-}
  forMemory = memory

instance HasCPU DebugState where
  {-# INLINE forCPUState #-}
  forCPUState = cpu

instance HasKeypad DebugState where
  {-# INLINE forKeypadState #-}
  forKeypadState = keypadState

instance HasTimer DebugState where
  {-# INLINE forTimerState #-}
  forTimerState = timerState

instance HasGraphics DebugState where
  {-# INLINE forGraphicsState #-}
  forGraphicsState = graphicsState
  {-# INLINE forGraphicsSync #-}
  forGraphicsSync = graphicsSync

instance HasBus DebugState where
  {-# INLINE forBusState #-}
  forBusState = bus

type MonadDebug a = ReaderT DebugState IO a

-- | Initialise the debugger.
initDebug :: FilePath -> CPUState -> Memory -> GraphicsSync -> IO DebugState
initDebug romFile cpu memory graphicsSync = do
  codeMap            <- newIORef =<< initMap romFile
  executeBreakpoints <- initBreakpointTable
  writeBreakpoints   <- initBreakpointTable
  bus                <- initBusState
  graphicsState      <- initGraphics
  keypadState        <- initKeypadState
  lcdEnabled         <- newIORef True
  timerState         <- initTimerState
  pure DebugState { .. }

getCodeMap :: MonadDebug SymbolTable
getCodeMap = liftIO . readIORef =<< asks codeMap

setCodeMap :: SymbolTable -> MonadDebug ()
setCodeMap value = liftIO . (`writeIORef` value) =<< asks codeMap

-- | Create a function to decorate disassembly dumps. Adds symbols to indicate
-- breakpoints, PC and other information.
makeDecorator :: MonadDebug (Word16 -> IO String)
makeDecorator = do
  DebugState {..} <- ask
  pc              <- readPC
  pure $ \addr -> do
    bp <- getBreakpoint executeBreakpoints addr <&> \case
      Nothing    -> ' '
      Just True  -> '*'
      Just False -> '-'
    pure [bp, if addr == pc then '>' else ' ']

-- | A condition to check before executing the next instruction. Break if the
-- condition is True.
type BreakPreCondition = MonadDebug Bool

-- | A condition to check after executing the next instruction. Break if the
-- condition is True.
type BreakPostCondition = BusEvent -> MonadDebug Bool

-- | Break after a certain number of instructions have been executed.
breakOnCountOf :: Int -> MonadDebug BreakPreCondition
breakOnCountOf n0 = do
  counter <- liftIO (newIORef n0)
  pure . liftIO $ do
    c <- readIORef counter
    if c == 0
      then pure True
      else do
        writeIORef counter (c - 1)
        pure False

-- | Break when breakpoints are triggered.
breakOnBreakpoints :: MonadDebug BreakPostCondition
breakOnBreakpoints = do
  DebugState {..} <- ask
  pure $ \event -> do
    breakOnExecute <- shouldBreakOnExecute executeBreakpoints
    breakOnWrite   <- shouldBreakOnWrite writeBreakpoints event
    pure (breakOnExecute || breakOnWrite)

-- | Break when the PC equals a certain value.
breakOnPC :: Word16 -> BreakPostCondition
breakOnPC pc = const ((pc ==) <$> readPC)

-- | Break when a RET instruction is executed with the current stack pointer.
breakOnRet :: Word16 -> BreakPreCondition
breakOnRet originalSP = do
  sp          <- readR16 RegSP
  instruction <- decodeOnly decode
  case instruction of
    RET               -> pure (originalSP == sp)
    (RETCC condition) -> do
      shouldRet <- testCondition condition
      pure (shouldRet && originalSP == sp)
    RETI -> pure (originalSP == sp)
    _    -> pure False

-- | Run the interpreter until one of the break conditions is met.
doRun :: [BreakPreCondition] -> [BreakPostCondition] -> MonadDebug Int
doRun preConditions postConditions = clearBreakFlag >> go 0
 where
  go !ticks = do
    doPreBreak <- orM preConditions
    if doPreBreak
      then pure ticks
      else do
        debugInfo <- busStep
        let ticks' = ticks + clockAdvance debugInfo
        breakFlag   <- isBreakFlagSet
        doPostBreak <- orM (fmap ($ debugInfo) postConditions)
        if doPostBreak || breakFlag || currentMode debugInfo == ModeStop
          then pure ticks'
          else go ticks'

-- | Disassemble the next 4 instructions at the current PC.
disassembleAtPC :: MonadDebug ()
disassembleAtPC = do
  symbols   <- getCodeMap
  decorator <- makeDecorator
  pc        <- readPC
  dumpDisassembly decorator symbols pc 4

withAddress :: MemAddress -> (Word16 -> MonadDebug ()) -> MonadDebug ()
withAddress (ConstAddress addr              ) action = action addr
withAddress (LabelAddress label maybeAddress) action = do
  symbols <- getCodeMap
  case lookupBySymbol symbols label of
    Just addr -> action addr
    Nothing   -> case maybeAddress of
      Just address -> action address
      Nothing      -> liftIO (putStrLn ("Unknown symbol " ++ label))

reportingClockStats :: MonadIO m => m Int -> m ()
reportingClockStats action = do
  t0     <- SDL.ticks
  cycles <- action
  t1     <- SDL.ticks
  let duration       = t1 - t0
  let expectedCycles = (4194304.0 * fromIntegral duration) / 1000.0 :: Double
  let percentSpeed = (fromIntegral cycles / expectedCycles) * 100.0

  when (duration > 0) $ liftIO $ do
    putStrLn (show cycles ++ " cycles in " ++ show duration ++ "ms")
    putStrLn ("Clock rate = " ++ show ((cycles * 1000) `div` fromIntegral duration) ++ "Hz")
    putStrLn ("Relative to expected clock " ++ showFFloat (Just 2) percentSpeed "" ++ "% speed")

-- | Execute a debugger command.
doCommand :: Command -> MonadDebug ()
doCommand ShowHeader = do
  mem <- asks forMemory
  liftIO (dumpHeader (getROMHeader mem))
doCommand ShowRegs = do
  registerFile <- getRegisterFile
  liftIO (dumpRegisters registerFile)
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
  reportingClockStats (doRun preConditions postConditions)
  disassembleAtPC
doCommand StepOut = do
  sp             <- readR16 RegSP
  postConditions <- sequence [breakOnBreakpoints]
  reportingClockStats (doRun [breakOnRet sp] postConditions)
  DebugState {..} <- ask
  wasBreakpoint   <- shouldBreakOnExecute executeBreakpoints
  unless wasBreakpoint (void busStep)
  disassembleAtPC
doCommand Run = do
  postConditions <- sequence [breakOnBreakpoints]
  reportingClockStats (doRun [] postConditions)
  disassembleAtPC
doCommand (RunTo breakAddr) = withAddress breakAddr $ \addr -> do
  postConditions <- sequence [breakOnBreakpoints, pure (breakOnPC addr)]
  reportingClockStats (doRun [] postConditions)
  disassembleAtPC
doCommand Reset               = reset
doCommand (Poke8  addr value) = withAddress addr $ \actualAddr -> writeByte actualAddr value
doCommand (Poke16 addr value) = withAddress addr $ \actualAddr -> writeWord actualAddr value
doCommand (Peek8 addr) =
  withAddress addr $ \actualAddr -> liftIO . putStrLn . formatHex =<< readByte actualAddr
doCommand (Peek16 addr) = withAddress addr $ \actualAddr -> do
  l <- readByte actualAddr
  h <- readByte (actualAddr + 1)
  liftIO . putStrLn $ formatHex ((fromIntegral h `shiftL` 8) .|. fromIntegral l :: Word16)
doCommand (PokeR8  reg value ) = writeR8 reg value
doCommand (PokeR16 reg value ) = writeR16 reg value
doCommand (ShowGraphics  r   ) = dumpGraphics r
doCommand (ShowTimer     r   ) = dumpTimer r
doCommand (ShowAudio     r   ) = dumpAudio r
doCommand (ShowInternal  r   ) = dumpInternal r
doCommand (ShowMBC       r   ) = dumpMBC r
doCommand (AddBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- ask
  liftIO (setBreakpoint executeBreakpoints breakAddr True)
doCommand (DeleteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- ask
  liftIO (clearBreakpoint executeBreakpoints breakAddr)
doCommand (DisableBreakpiont addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- ask
  liftIO (setBreakpoint executeBreakpoints breakAddr False)
doCommand (AddWriteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- ask
  liftIO (setBreakpoint writeBreakpoints breakAddr True)
doCommand (DeleteWriteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- ask
  liftIO (clearBreakpoint writeBreakpoints breakAddr)
doCommand (DisableWriteBreakpiont addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- ask
  liftIO (setBreakpoint writeBreakpoints breakAddr False)
doCommand ListBreakpoints = do
  DebugState {..} <- ask
  liftIO $ do
    allExecuteBreakpoints <- listBreakpoints executeBreakpoints
    allWriteBreakpoints   <- listBreakpoints writeBreakpoints
    when (null allExecuteBreakpoints && null allWriteBreakpoints) $ putStrLn "No breakpoints set"
    for_ allExecuteBreakpoints
      $ \(addr, enabled) -> putStrLn (formatHex addr ++ if not enabled then " (disabled)" else "")
    for_ allWriteBreakpoints $ \(addr, enabled) ->
      putStrLn ("On write to " ++ formatHex addr ++ if not enabled then " (disabled)" else "")
doCommand (AddSymbol symbol value) = do
  DebugState {..} <- ask
  codeMap'        <- addToMap (symbol, value) <$> getCodeMap
  liftIO (saveMap (mapFileName romFile) codeMap')
  setCodeMap codeMap'
doCommand (DeleteSymbol symbol) = do
  DebugState {..} <- ask
  codeMap'        <- removeFromMap symbol <$> getCodeMap
  liftIO (saveMap (mapFileName romFile) codeMap')
  setCodeMap codeMap'
doCommand ListSymbols = do
  symbols <- listSymbols <$> getCodeMap
  for_ symbols $ \(label, value) -> liftIO (putStrLn (label ++ ": " ++ formatHex value))
