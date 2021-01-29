{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Primitive
  ( Counter,
    newCounter,
    getCounter,
    reloadCounter,
    updateCounter,
    UpdateResult (..),
    StateCycle,
    newStateCycle,
    getStateCycle,
    getUpdateResult,
    updateStateCycle,
    resetStateCycle,
    RingBuffer,
    newRingBuffer,
    readableSize,
    writableSize,
    writeBuffer,
    foldBuffer,
    LinearFeedbackShiftRegister,
    newLinearFeedbackShiftRegister,
    initLinearFeedbackShiftRegister,
    nextBit,
    currentBit,
    Port,
    newPort,
    newPortWithReadAction,
    newPortWithReadMask,
    alwaysUpdate,
    neverUpdate,
    readPort,
    directReadPort,
    writePort,
    directWritePort,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits (Bits (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive (Prim)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Storable (Storable (peekElemOff, pokeElemOff))
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)

-- | A reloading down counter.  Number of states is reload value + 1.
data Counter = Counter
  { counterMask :: !Int,
    counterValue :: !(UnboxedRef Int)
  }

newCounter :: MonadIO m => Int -> m Counter
newCounter counterMask = do
  counterValue <- newUnboxedRef 0
  pure Counter {..}

{-# INLINE reloadCounter #-}
reloadCounter :: MonadIO m => Counter -> Int -> m ()
reloadCounter Counter {..} reload =
  writeUnboxedRef counterValue (if reload == 0 then counterMask + 1 else reload)

{-# INLINE getCounter #-}
getCounter :: MonadIO m => Counter -> m Int
getCounter Counter {..} = do
  v <- readUnboxedRef counterValue
  pure (v .&. counterMask)

{-# INLINE updateCounter #-}
updateCounter :: MonadIO m => Counter -> Int -> m Int -> m ()
updateCounter Counter {..} update getReloadValue = do
  count <- readUnboxedRef counterValue
  if update < count
    then writeUnboxedRef counterValue (count - update)
    else do
      reload <- getReloadValue
      let period = if reload == 0 then counterMask + 1 else reload
      writeUnboxedRef counterValue (count - update + period)

data StateCycle a = StateCycle !(UnboxedRef Int) !(IORef [(a, Int)])

newStateCycle :: MonadIO m => [(a, Int)] -> m (StateCycle a)
newStateCycle [] = error "Tried to create a counter with no states!"
newStateCycle states@((_, count0) : _) =
  StateCycle <$> newUnboxedRef count0 <*> liftIO (newIORef (cycle states))

{-# INLINE getStateCycle #-}
getStateCycle :: (MonadIO m, MonadFail m) => StateCycle a -> m a
getStateCycle (StateCycle _ states) = do
  ((state, _) : _) <- liftIO $ readIORef states
  pure state

data UpdateResult a = NoChange !a | HasChangedTo !a deriving (Eq, Ord, Show)

getUpdateResult :: UpdateResult a -> a
getUpdateResult (NoChange x) = x
getUpdateResult (HasChangedTo x) = x

{-# INLINE updateStateCycle #-}
updateStateCycle :: MonadIO m => StateCycle a -> Int -> (a -> m ()) -> m (UpdateResult a)
updateStateCycle (StateCycle cycles states) update k = do
  count <- readUnboxedRef cycles
  let count' = count - update
  if count' > 0
    then do
      writeUnboxedRef cycles count'
      NoChange . fst . head <$> liftIO (readIORef states)
    else do
      stateList <- liftIO $ readIORef states
      let stateList' = tail stateList
      let (nextState, nextStateLength) = head stateList'
      liftIO $ writeIORef states $! stateList'
      writeUnboxedRef cycles (count' + nextStateLength)
      k nextState
      pure (HasChangedTo nextState)

{-# INLINE resetStateCycle #-}
resetStateCycle :: MonadIO m => StateCycle a -> [(a, Int)] -> m ()
resetStateCycle (StateCycle cycles states) states' = case states' of
  [] -> error "Tried to reset a counter with no states!"
  ((_, count0) : _) -> do
    liftIO (writeIORef states $! cycle states')
    writeUnboxedRef cycles count0

data RingBuffer a = RingBuffer
  { ringMask :: {-# UNPACK #-} !Int,
    ringBuffer :: !(ForeignPtr a),
    ringReadPtr :: !(UnboxedRef Int),
    ringWritePtr :: !(UnboxedRef Int)
  }

newRingBuffer :: Storable a => Int -> IO (RingBuffer a)
newRingBuffer size = do
  ringBuffer <- mallocForeignPtrArray (2 ^ size)
  ringReadPtr <- newUnboxedRef 0
  ringWritePtr <- newUnboxedRef 0
  let ringMask = (2 ^ size) - 1
  pure RingBuffer {..}

{-# INLINE readableSize #-}
readableSize :: RingBuffer a -> IO Int
readableSize RingBuffer {..} = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  pure $ writePtr - readPtr

{-# INLINE writableSize #-}
writableSize :: RingBuffer a -> IO Int
writableSize RingBuffer {..} = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  pure $ (ringMask + 1) - (writePtr - readPtr)

{-# INLINE writeBuffer #-}
writeBuffer :: Storable a => RingBuffer a -> a -> IO ()
writeBuffer RingBuffer {..} x = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  when ((writePtr - readPtr) <= ringMask) $ do
    withForeignPtr ringBuffer $ \ptr -> pokeElemOff ptr (writePtr .&. ringMask) x
    writeUnboxedRef ringWritePtr (writePtr + 1)

{-# INLINEABLE foldBuffer #-}
foldBuffer :: Storable a => RingBuffer a -> Int -> b -> (b -> a -> IO b) -> IO b
foldBuffer RingBuffer {..} limit acc0 accumulate = do
  readPtr <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  let nextReadPtr = writePtr `min` (readPtr + limit)
  writeUnboxedRef ringReadPtr nextReadPtr
  withForeignPtr ringBuffer $ \base ->
    let go !acc !i =
          if i == nextReadPtr
            then pure acc
            else do
              acc' <- accumulate acc =<< peekElemOff base (i .&. ringMask)
              go acc' (i + 1)
     in go acc0 readPtr

newtype LinearFeedbackShiftRegister a = LinearFeedbackShiftRegister (UnboxedRef a)

newLinearFeedbackShiftRegister :: (Prim a, Bits a) => IO (LinearFeedbackShiftRegister a)
newLinearFeedbackShiftRegister = LinearFeedbackShiftRegister <$> newUnboxedRef (bit 0)

{-# INLINE initLinearFeedbackShiftRegister #-}
{-# SPECIALIZE initLinearFeedbackShiftRegister :: Word8 -> LinearFeedbackShiftRegister Word8 -> IO () #-}
initLinearFeedbackShiftRegister :: Prim a => a -> LinearFeedbackShiftRegister a -> IO ()
initLinearFeedbackShiftRegister value (LinearFeedbackShiftRegister ref) = writeUnboxedRef ref value

{-# INLINE nextBit #-}
{-# SPECIALIZE nextBit :: LinearFeedbackShiftRegister Word8 -> Word8 -> IO Word8 #-}
nextBit :: (Prim a, Num a, Bits a) => LinearFeedbackShiftRegister a -> a -> IO a
nextBit (LinearFeedbackShiftRegister ref) mask = do
  register <- readUnboxedRef ref
  let shifted = register `unsafeShiftR` 1
  let register' =
        (shifted .&. complement mask) .|. (mask .&. negate (1 .&. (register `xor` shifted)))
  writeUnboxedRef ref register'
  pure register'

{-# INLINE currentBit #-}
currentBit :: (Prim a) => LinearFeedbackShiftRegister a -> IO a
currentBit (LinearFeedbackShiftRegister ref) = readUnboxedRef ref

-- | A port is like an IORef but with a custom handler for writes.
data Port = Port
  { portWriteMask :: !Word8,
    portValue :: !(UnboxedRef Word8),
    portRead :: !(Word8 -> IO Word8),
    portNotify :: !(Word8 -> Word8 -> IO Word8)
  }

-- | Create a new port.
newPort ::
  -- | Initial value.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newPort value0 portWriteMask portNotify = do
  portValue <- newUnboxedRef value0
  let portRead = pure
  pure Port {..}

-- | Create a new port with a custom action to run when reading.
newPortWithReadAction ::
  -- | Initial value.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to perform on reads.
  (Word8 -> IO Word8) ->
  -- | Action to perform on writes.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newPortWithReadAction value0 portWriteMask portRead portNotify = do
  portValue <- newUnboxedRef value0
  pure Port {..}

-- | Create a new port.
newPortWithReadMask ::
  -- | Initial value.
  Word8 ->
  -- | Read mask.  1 indicates that the bit will always read as 1.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newPortWithReadMask value0 portReadMask portWriteMask portNotify = do
  portValue <- newUnboxedRef value0
  let portRead x = pure (x .|. portReadMask)
  pure Port {..}

{-# INLINEABLE alwaysUpdate #-}
alwaysUpdate :: Applicative f => a -> b -> f b
alwaysUpdate _ = pure

{-# INLINEABLE neverUpdate #-}
neverUpdate :: Applicative f => a -> b -> f a
neverUpdate = const . pure

-- | Read from the port
{-# INLINE readPort #-}
readPort :: MonadIO m => Port -> m Word8
readPort Port {..} = liftIO . portRead =<< readUnboxedRef portValue

-- | Read from the port directly skipping the read mask.
{-# INLINE directReadPort #-}
directReadPort :: MonadIO m => Port -> m Word8
directReadPort Port {..} = readUnboxedRef portValue

-- | Write to the port and notify any listeners.
{-# INLINE writePort #-}
writePort :: MonadIO m => Port -> Word8 -> m ()
writePort Port {..} newValue = do
  oldValue <- readUnboxedRef portValue
  newValue' <-
    liftIO
      (portNotify oldValue ((oldValue .&. complement portWriteMask) .|. newValue .&. portWriteMask))
  writeUnboxedRef portValue newValue'

-- | Write the value of the port directly without any checks or notifications.
{-# INLINE directWritePort #-}
directWritePort :: MonadIO m => Port -> Word8 -> m ()
directWritePort Port {..} v = liftIO (writeUnboxedRef portValue v)
