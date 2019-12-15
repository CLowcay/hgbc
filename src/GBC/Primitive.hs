{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}

module GBC.Primitive
  ( Counter
  , newCounter
  , getCounter
  , reloadCounter
  , updateCounter
  , UpdateResult(..)
  , StateCycle
  , newStateCycle
  , getStateCycle
  , getUpdateResult
  , updateStateCycle
  , resetStateCycle
  , RingBuffer
  , newRingBuffer
  , readableSize
  , writableSize
  , writeBuffer
  , foldBuffer
  , LinearFeedbackShiftRegister
  , newLinearFeedbackShiftRegister
  , initLinearFeedbackShiftRegister
  , nextBit
  , Port
  , newPort
  , newPortWithReadMask
  , alwaysUpdate
  , neverUpdate
  , readPort
  , directReadPort
  , writePort
  , directWritePort
  , setPortBits
  , clearPortBits
  )
where

import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.IORef
import           Data.Primitive
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GBC.Primitive.UnboxedRef

-- | A reloading down counter.  Number of states is reload value + 1.
newtype Counter = Counter (UnboxedRef Int)

newCounter :: MonadIO m => m Counter
newCounter = liftIO $ Counter <$> newUnboxedRef 0

{-# INLINE reloadCounter #-}
reloadCounter :: MonadIO m => Counter -> Int -> m ()
reloadCounter (Counter ref) reload = liftIO $ writeUnboxedRef ref reload

{-# INLINE getCounter #-}
getCounter :: MonadIO m => Counter -> m Int
getCounter (Counter ref) = liftIO $ readUnboxedRef ref

{-# INLINE updateCounter #-}
updateCounter :: MonadIO m => Counter -> Int -> m Int -> m ()
updateCounter (Counter ref) update k = do
  count <- liftIO $ readUnboxedRef ref
  let count' = count - update
  if count' >= 0
    then liftIO $ writeUnboxedRef ref count'
    else do
      reload <- k
      liftIO $ writeUnboxedRef ref (count' + reload + 1)

data StateCycle a = StateCycle !(UnboxedRef Int) !(IORef [(a, Int)])

newStateCycle :: MonadIO m => [(a, Int)] -> m (StateCycle a)
newStateCycle [] = error "Tried to create a counter with no states!"
newStateCycle states@((_, count0) : _) =
  liftIO $ StateCycle <$> newUnboxedRef count0 <*> newIORef (cycle states)

{-# INLINE getStateCycle #-}
getStateCycle :: (MonadIO m, MonadFail m) => StateCycle a -> m a
getStateCycle (StateCycle _ states) = do
  ((state, _) : _) <- liftIO $ readIORef states
  pure state

data UpdateResult a = NoChange !a | HasChangedTo !a deriving (Eq, Ord, Show)

getUpdateResult :: UpdateResult a -> a
getUpdateResult (NoChange     x) = x
getUpdateResult (HasChangedTo x) = x

{-# INLINE updateStateCycle #-}
updateStateCycle :: MonadIO m => StateCycle a -> Int -> (a -> m ()) -> m (UpdateResult a)
updateStateCycle (StateCycle cycles states) update k = do
  count <- liftIO $ readUnboxedRef cycles
  let count' = count - update
  if count' > 0
    then liftIO $ do
      writeUnboxedRef cycles count'
      NoChange . fst . head <$> readIORef states
    else do
      stateList <- liftIO $ readIORef states
      let stateList'                   = tail stateList
      let (nextState, nextStateLength) = head stateList'
      liftIO $ writeIORef states $! stateList'
      liftIO $ writeUnboxedRef cycles (count' + nextStateLength)
      k nextState
      pure (HasChangedTo nextState)

{-# INLINE resetStateCycle #-}
resetStateCycle :: MonadIO m => StateCycle a -> [(a, Int)] -> m ()
resetStateCycle (StateCycle cycles states) states' = case states' of
  []                -> error "Tried to reset a counter with no states!"
  ((_, count0) : _) -> liftIO $ do
    writeIORef states $! cycle states'
    writeUnboxedRef cycles count0

data RingBuffer a = RingBuffer {
    ringBuffer :: !(ForeignPtr a)
  , ringReadPtr :: !(UnboxedRef Int)
  , ringWritePtr :: !(UnboxedRef Int)
  , ringMask :: !Int
}

newRingBuffer :: Storable a => Int -> IO (RingBuffer a)
newRingBuffer size = do
  ringBuffer   <- mallocForeignPtrArray (2 ^ size)
  ringReadPtr  <- newUnboxedRef 0
  ringWritePtr <- newUnboxedRef 0
  let ringMask = (2 ^ size) - 1
  pure RingBuffer { .. }

{-# INLINE readableSize #-}
readableSize :: RingBuffer a -> IO Int
readableSize RingBuffer {..} = do
  readPtr  <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  pure $ writePtr - readPtr

{-# INLINE writableSize #-}
writableSize :: RingBuffer a -> IO Int
writableSize RingBuffer {..} = do
  readPtr  <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  pure $ (ringMask + 1) - (writePtr - readPtr)

{-# INLINE writeBuffer #-}
writeBuffer :: Storable a => RingBuffer a -> a -> IO ()
writeBuffer RingBuffer {..} x = do
  readPtr  <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  when ((writePtr - readPtr) <= ringMask) $ do
    withForeignPtr ringBuffer $ \ptr -> pokeElemOff ptr (writePtr .&. ringMask) x
    writeUnboxedRef ringWritePtr (writePtr + 1)

{-# INLINABLE foldBuffer #-}
foldBuffer :: Storable a => RingBuffer a -> Int -> b -> (b -> a -> IO b) -> IO b
foldBuffer RingBuffer {..} limit acc0 accumulate = do
  readPtr  <- readUnboxedRef ringReadPtr
  writePtr <- readUnboxedRef ringWritePtr
  let nextReadPtr = writePtr `min` (readPtr + limit)
  writeUnboxedRef ringReadPtr nextReadPtr
  withForeignPtr ringBuffer $ \base ->
    let go !acc !i = if i == nextReadPtr
          then pure acc
          else do
            acc' <- accumulate acc =<< peekElemOff base (i .&. ringMask)
            go acc' (i + 1)
    in  go acc0 readPtr

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

-- | A port is like an IORef but with a custom handler for writes.
data Port a = Port {
    portValue     :: !(UnboxedRef a)
  , portReadMask  :: !a
  , portWriteMask :: !a
  , portNotify    :: !(a -> a -> IO a)
}

-- | Create a new port.
newPort
  :: (Prim a, Num a)
  => a                  -- ^ Initial value.
  -> a                  -- ^ Write mask.  1 indicates that the bit is writable.
  -> (a -> a -> IO a)   -- ^ Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  -> IO (Port a)
newPort value0 portWriteMask portNotify = do
  portValue <- newUnboxedRef value0
  let portReadMask = 0x00
  pure Port { .. }

-- | Create a new port.
newPortWithReadMask
  :: Prim a
  => a                  -- ^ Initial value.
  -> a                  -- ^ Read mask.  1 indicates that the bit will always read as 1.
  -> a                  -- ^ Write mask.  1 indicates that the bit is writable.
  -> (a -> a -> IO a)   -- ^ Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  -> IO (Port a)
newPortWithReadMask value0 portReadMask portWriteMask portNotify = do
  portValue <- newUnboxedRef value0
  pure Port { .. }

{-# INLINABLE alwaysUpdate #-}
alwaysUpdate :: Applicative f => a -> b -> f b
alwaysUpdate _ = pure

{-# INLINABLE neverUpdate #-}
neverUpdate :: Applicative f => a -> b -> f a
neverUpdate = const . pure

-- | Read from the port
{-# INLINE readPort #-}
{-# SPECIALIZE readPort :: Port Word8 -> IO Word8 #-}
readPort :: (Prim a, Bits a) => Port a -> IO a
readPort Port {..} = do
  value <- readUnboxedRef portValue
  pure (value .|. portReadMask)

-- | Read from the port directly skipping the read mask.
{-# INLINE directReadPort #-}
{-# SPECIALIZE directReadPort :: Port Word8 -> IO Word8 #-}
directReadPort :: Prim a => Port a -> IO a
directReadPort Port {..} = readUnboxedRef portValue

-- | Write to the port and notify any listeners.
{-# INLINE writePort #-}
{-# SPECIALIZE writePort :: Port Word8 -> Word8 ->IO () #-}
writePort :: (Prim a, Bits a) => Port a -> a -> IO ()
writePort Port {..} newValue = do
  oldValue  <- readUnboxedRef portValue
  newValue' <- portNotify oldValue
                          ((oldValue .&. complement portWriteMask) .|. newValue .&. portWriteMask)
  writeUnboxedRef portValue newValue'

-- | Write the value of the port directly without any checks or notifications.
{-# INLINE directWritePort #-}
{-# SPECIALIZE directWritePort :: Port Word8 -> Word8 ->IO () #-}
directWritePort :: Prim a => Port a -> a -> IO ()
directWritePort Port {..} = writeUnboxedRef portValue

-- | Set writable bits and notify all listeners.
{-# INLINE setPortBits #-}
{-# SPECIALIZE setPortBits :: Port Word8 -> Word8 ->IO () #-}
setPortBits :: (Prim a, Bits a) => Port a -> a -> IO ()
setPortBits Port {..} newValue = do
  oldValue  <- readUnboxedRef portValue
  newValue' <- portNotify oldValue (oldValue .|. (newValue .&. portWriteMask))
  writeUnboxedRef portValue newValue'

-- | Set writable bits and notify all listeners.
{-# INLINE clearPortBits #-}
{-# SPECIALIZE clearPortBits :: Port Word8 -> Word8 ->IO () #-}
clearPortBits :: (Prim a, Bits a) => Port a -> a -> IO ()
clearPortBits Port {..} newValue = do
  oldValue  <- readUnboxedRef portValue
  newValue' <- portNotify oldValue (oldValue .&. complement (newValue .&. portWriteMask))
  writeUnboxedRef portValue newValue'
