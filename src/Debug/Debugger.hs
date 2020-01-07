{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

module Debug.Debugger
  ( Command(..)
  , MemAddress(..)
  , Option(..)
  , DebugState
  , initDebug
  , doCommand
  )
where

import           Common
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Word
import           Debug.Breakpoints
import           Debug.Dump
import           Debug.Map
import           GBC.Audio
import           GBC.Emulator
import           GBC.CPU
import           GBC.CPU.ISA
import           GBC.Memory
import           GBC.ROM
import           Numeric
import           System.Console.Haskeline
import qualified SDL.Time                      as SDL

data MemAddress = ConstAddress !Word16 | LabelAddress !String !(Maybe Word16) deriving (Eq, Ord, Show)

data Option = CheckRAMAccess deriving (Eq, Ord, Show)

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
             | EnableOption Option
             | DisableOption Option
             deriving (Eq, Ord, Show)

-- | The current state of the debugger.
data DebugState = DebugState {
    emulator :: !EmulatorState
  , romFileName :: !FilePath
  , codeMap :: !(IORef SymbolTable)
  , breakpoints :: !BreakpointTable
  , watchpoints :: !BreakpointTable
}

type MonadDebug a = ReaderT DebugState IO a
type MonadDebugger a = InputT (ReaderT DebugState IO) a

instance HasMemory DebugState where
  {-# INLINE forMemory #-}
  forMemory = forMemory . emulator

instance HasCPU DebugState where
  {-# INLINE forCPUState #-}
  forCPUState = forCPUState . emulator

-- | Initialise the debugger.
initDebug :: ROM -> EmulatorState -> IO DebugState
initDebug rom emulator = do
  codeMap     <- newIORef =<< initMap (romFile rom)
  breakpoints <- initBreakpointTable
  watchpoints <- initBreakpointTable
  let romFileName = romFile rom
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
    bp <- getBreakpoint breakpoints addr <&> \case
      Nothing    -> ' '
      Just True  -> '*'
      Just False -> '-'
    pure [bp, if addr == pc then '>' else ' ']

-- | A condition to check before executing the next instruction. Break if the
-- condition is True.
type BreakPreCondition = MonadDebug Bool

-- | A condition to check after executing the next instruction. Break if the
-- condition is True.
type BreakPostCondition = MonadDebug Bool

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
  pure $ do
    breakOnExecute <- shouldBreakOnExecute breakpoints
    --breakOnWrite   <- shouldBreakOnWrite watchpoints event
    --pure (breakOnExecute || breakOnWrite)
    pure breakOnExecute

-- | Break when the PC equals a certain value.
breakOnPC :: Word16 -> BreakPostCondition
breakOnPC pc = (pc ==) <$> readPC

-- | Break when a RET instruction is executed with the current stack pointer.
-- TODO: fix this
--breakOnRet :: Word16 -> BreakPreCondition
--breakOnRet originalSP = do
--  sp          <- readR16 RegSP
--  instruction <- decodeOnly decode
--  case instruction of
--    RET               -> pure (originalSP == sp)
--    (RETCC condition) -> do
--      shouldRet <- testCondition condition
--      pure (shouldRet && originalSP == sp)
--    RETI -> pure (originalSP == sp)
--    _    -> pure False

-- | Run the interpreter until one of the break conditions is met.
doRun :: [BreakPreCondition] -> [BreakPostCondition] -> MonadDebug ()
doRun preConditions postConditions = withReaderT emulator clearBreakFlag >> go
 where
  go = do
    doPreBreak <- orM preConditions
    if doPreBreak
      then pure ()
      else do
        withReaderT emulator step
        breakFlag   <- withReaderT emulator isBreakFlagSet
        doPostBreak <- orM postConditions
        if doPostBreak || breakFlag then pure () else go

-- | Disassemble the next 4 instructions at the current PC.
disassembleAtPC :: MonadDebugger ()
disassembleAtPC = do
  symbols   <- lift getCodeMap
  decorator <- lift makeDecorator
  pc        <- lift readPC
  dumpDisassembly decorator symbols pc 4

withAddress :: MemAddress -> (Word16 -> MonadDebugger ()) -> MonadDebugger ()
withAddress (ConstAddress addr              ) action = action addr
withAddress (LabelAddress label maybeAddress) action = do
  symbols <- lift getCodeMap
  case lookupBySymbol symbols label of
    Just addr -> action addr
    Nothing   -> case maybeAddress of
      Just address -> action address
      Nothing      -> liftIO (putStrLn ("Unknown symbol " ++ label))

reportingClockStats :: MonadDebugger () -> MonadDebugger ()
reportingClockStats action = do
  clocks0 <- lift $ withReaderT emulator getEmulatorClock
  t0      <- SDL.ticks
  action
  t1      <- SDL.ticks
  clocks1 <- lift $ withReaderT emulator getEmulatorClock

  let duration       = t1 - t0
  let clocks         = clocks1 - clocks0

  let expectedCycles = (4194304.0 * fromIntegral duration) / 1000.0 :: Double
  let percentSpeed = (fromIntegral clocks / expectedCycles) * 100.0

  when (duration > 0) $ do
    outputStrLn (show clocks ++ " cycles in " ++ show duration ++ "ms")
    outputStrLn ("Clock rate = " ++ show ((clocks * 1000) `div` fromIntegral duration) ++ "Hz")
    outputStrLn ("Relative to expected clock " ++ showFFloat (Just 2) percentSpeed "" ++ "% speed")

-- | Execute a debugger command.
doCommand :: Command -> MonadDebugger ()
doCommand ShowHeader = do
  mem <- lift (asks forMemory)
  dumpHeader (getROMHeader mem)
doCommand ShowRegs                  = dumpRegisters
doCommand (ShowMem         addr   ) = withAddress addr dumpMem
doCommand (ShowDisassembly Nothing) = do
  pc        <- lift readPC
  decorator <- lift makeDecorator
  symbols   <- lift getCodeMap
  dumpDisassembly decorator symbols pc 10
doCommand (ShowDisassembly (Just addr)) = do
  decorator <- lift makeDecorator
  symbols   <- lift getCodeMap
  withAddress addr $ \actualAddress -> dumpDisassembly decorator symbols actualAddress 10
doCommand (Step n) = do
  preConditions  <- lift (sequence [breakOnCountOf n])
  postConditions <- lift (sequence [breakOnBreakpoints])
  reportingClockStats (lift (doRun preConditions postConditions))
  disassembleAtPC
doCommand StepOut = do
  -- TODO: fix this
  --sp             <- lift (readR16 RegSP)
  --postConditions <- lift (sequence [breakOnBreakpoints])
  --reportingClockStats (lift (doRun [breakOnRet sp] postConditions))
  --lift $ do
  --  DebugState { breakpoints } <- ask
  --  wasBreakpoint              <- shouldBreakOnExecute breakpoints
  --  unless wasBreakpoint (void (withReaderT emulator step))
  disassembleAtPC
doCommand Run = do
  postConditions     <- lift (sequence [breakOnBreakpoints])
  EmulatorState {..} <- lift (asks emulator)
  bracket (liftIO $ enableAudioOut audioState) (const $ liftIO $ disableAudioOut audioState)
    $ const
    $ reportingClockStats (lift (doRun [] postConditions))
  disassembleAtPC
doCommand (RunTo breakAddr) = withAddress breakAddr $ \addr -> do
  postConditions <- lift (sequence [breakOnBreakpoints, pure (breakOnPC addr)])
  reportingClockStats (lift (doRun [] postConditions))
  disassembleAtPC
doCommand Reset               = lift reset
doCommand (Poke8  addr value) = withAddress addr $ \actualAddr -> lift (writeByte actualAddr value)
doCommand (Poke16 addr value) = withAddress addr $ \actualAddr -> lift (writeWord actualAddr value)
doCommand (Peek8 addr) =
  withAddress addr $ \actualAddr -> liftIO . putStrLn . formatHex =<< lift (readByte actualAddr)
doCommand (Peek16 addr) = withAddress addr $ \actualAddr -> do
  l <- lift (readByte actualAddr)
  h <- lift (readByte (actualAddr + 1))
  outputStrLn (formatHex ((fromIntegral h `shiftL` 8) .|. fromIntegral l :: Word16))
doCommand (PokeR8  reg value) = lift (writeR8 reg value)
doCommand (PokeR16 reg value) = lift (writeR16 reg value)
doCommand (ShowGraphics r   ) = do
  graphics <- lift (asks (graphicsState . emulator))
  dumpGraphics graphics r
doCommand (ShowTimer r) = do
  timer <- lift (asks (timerState . emulator))
  dumpTimer timer r
doCommand (ShowInternal  r   ) = dumpInternal r
doCommand (ShowMBC       r   ) = dumpMBC r
doCommand (AddBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- lift ask
  liftIO (setBreakpoint breakpoints breakAddr True)
doCommand (DeleteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- lift ask
  liftIO (clearBreakpoint breakpoints breakAddr)
doCommand (DisableBreakpiont addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- lift ask
  liftIO (setBreakpoint breakpoints breakAddr False)
doCommand (AddWriteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- lift ask
  liftIO (setBreakpoint watchpoints breakAddr True)
doCommand (DeleteWriteBreakpoint addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- lift ask
  liftIO (clearBreakpoint watchpoints breakAddr)
doCommand (DisableWriteBreakpiont addr) = withAddress addr $ \breakAddr -> do
  DebugState {..} <- lift ask
  liftIO (setBreakpoint watchpoints breakAddr False)
doCommand ListBreakpoints = do
  DebugState {..}       <- lift ask
  allExecuteBreakpoints <- liftIO (listBreakpoints breakpoints)
  allWriteBreakpoints   <- liftIO (listBreakpoints watchpoints)
  when (null allExecuteBreakpoints && null allWriteBreakpoints) $ outputStrLn "No breakpoints set"
  for_ allExecuteBreakpoints $ \(addr, enabled) ->
    outputStrLn (formatHex addr ++ if not enabled then " (disabled)" else "")
  for_ allWriteBreakpoints $ \(addr, enabled) ->
    outputStrLn ("On write to " ++ formatHex addr ++ if not enabled then " (disabled)" else "")
doCommand (AddSymbol symbol value) = lift $ do
  DebugState {..} <- ask
  codeMap'        <- addToMap (symbol, value) <$> getCodeMap
  liftIO (saveMap (mapFileName romFileName) codeMap')
  setCodeMap codeMap'
doCommand (DeleteSymbol symbol) = lift $ do
  DebugState {..} <- ask
  codeMap'        <- removeFromMap symbol <$> getCodeMap
  liftIO (saveMap (mapFileName romFileName) codeMap')
  setCodeMap codeMap'
doCommand ListSymbols = do
  symbols <- lift (listSymbols <$> getCodeMap)
  for_ symbols $ \(label, value) -> outputStrLn (label ++ ": " ++ formatHex value)
doCommand (EnableOption  CheckRAMAccess) = lift (shouldCheckRAMAccess True)
doCommand (DisableOption CheckRAMAccess) = lift (shouldCheckRAMAccess False)
