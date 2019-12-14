{-# LANGUAGE BangPatterns #-}
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
import           Foreign.ForeignPtr
import           Foreign.Storable

-- | A reloading down counter.  Number of states is reload value + 1.
newtype Counter = Counter (IORef Int)

newCounter :: MonadIO m => m Counter
newCounter = liftIO $ Counter <$> newIORef 0

{-# INLINE reloadCounter #-}
reloadCounter :: MonadIO m => Counter -> Int -> m ()
reloadCounter (Counter ref) reload = liftIO $ writeIORef ref $! reload

{-# INLINE getCounter #-}
getCounter :: MonadIO m => Counter -> m Int
getCounter (Counter ref) = liftIO $ readIORef ref

{-# INLINE updateCounter #-}
updateCounter :: MonadIO m => Counter -> Int -> m Int -> m ()
updateCounter (Counter ref) update k = do
  count <- liftIO $ readIORef ref
  let count' = count - update
  if count' >= 0
    then liftIO $ writeIORef ref $! count'
    else do
      reload <- k
      liftIO $ writeIORef ref $! count' + reload + 1

data StateCycle a = StateCycle !(IORef Int) !(IORef [(a, Int)])

newStateCycle :: MonadIO m => [(a, Int)] -> m (StateCycle a)
newStateCycle [] = error "Tried to create a counter with no states!"
newStateCycle states@((_, count0) : _) =
  liftIO $ StateCycle <$> newIORef count0 <*> newIORef (cycle states)

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
  count <- liftIO $ readIORef cycles
  let count' = count - update
  if count' > 0
    then liftIO $ do
      writeIORef cycles $! count'
      NoChange . fst . head <$> readIORef states
    else do
      stateList <- liftIO $ readIORef states
      let stateList'                   = tail stateList
      let (nextState, nextStateLength) = head stateList'
      liftIO $ writeIORef states $! stateList'
      liftIO $ writeIORef cycles $! count' + nextStateLength
      k nextState
      pure (HasChangedTo nextState)

{-# INLINE resetStateCycle #-}
resetStateCycle :: MonadIO m => StateCycle a -> [(a, Int)] -> m ()
resetStateCycle (StateCycle cycles states) states' = case states' of
  []                -> error "Tried to reset a counter with no states!"
  ((_, count0) : _) -> liftIO $ do
    writeIORef states $! cycle states'
    writeIORef cycles $! count0

data RingBuffer a = RingBuffer {
    ringBuffer :: !(ForeignPtr a)
  , ringReadPtr :: !(IORef Int)
  , ringWritePtr :: !(IORef Int)
  , ringMask :: !Int
}

newRingBuffer :: Storable a => Int -> IO (RingBuffer a)
newRingBuffer size = do
  ringBuffer   <- mallocForeignPtrArray (2 ^ size)
  ringReadPtr  <- newIORef 0
  ringWritePtr <- newIORef 0
  let ringMask = (2 ^ size) - 1
  pure RingBuffer { .. }

{-# INLINE readableSize #-}
readableSize :: RingBuffer a -> IO Int
readableSize RingBuffer {..} = do
  readPtr  <- readIORef ringReadPtr
  writePtr <- readIORef ringWritePtr
  pure $ writePtr - readPtr

{-# INLINE writableSize #-}
writableSize :: RingBuffer a -> IO Int
writableSize RingBuffer {..} = do
  readPtr  <- readIORef ringReadPtr
  writePtr <- readIORef ringWritePtr
  pure $ (ringMask + 1) - (writePtr - readPtr)

{-# INLINE writeBuffer #-}
writeBuffer :: Storable a => RingBuffer a -> a -> IO ()
writeBuffer RingBuffer {..} x = do
  readPtr  <- readIORef ringReadPtr
  writePtr <- readIORef ringWritePtr
  when ((writePtr - readPtr) <= ringMask) $ do
    withForeignPtr ringBuffer $ \ptr -> pokeElemOff ptr (writePtr .&. ringMask) x
    writeIORef ringWritePtr (writePtr + 1)

{-# INLINABLE foldBuffer #-}
foldBuffer :: Storable a => RingBuffer a -> Int -> b -> (b -> a -> IO b) -> IO b
foldBuffer RingBuffer {..} limit acc0 accumulate = do
  readPtr  <- readIORef ringReadPtr
  writePtr <- readIORef ringWritePtr
  let nextReadPtr = writePtr `min` (readPtr + limit)
  writeIORef ringReadPtr $! nextReadPtr
  withForeignPtr ringBuffer $ \base ->
    let go !acc !i = if i == nextReadPtr
          then pure acc
          else do
            acc' <- accumulate acc =<< peekElemOff base (i .&. ringMask)
            go acc' (i + 1)
    in  go acc0 readPtr

newtype LinearFeedbackShiftRegister a = LinearFeedbackShiftRegister (IORef a)

newLinearFeedbackShiftRegister :: Bits a => IO (LinearFeedbackShiftRegister a)
newLinearFeedbackShiftRegister = LinearFeedbackShiftRegister <$> newIORef (bit 0)

{-# INLINE initLinearFeedbackShiftRegister #-}
initLinearFeedbackShiftRegister :: a -> LinearFeedbackShiftRegister a -> IO ()
initLinearFeedbackShiftRegister value (LinearFeedbackShiftRegister ref) = writeIORef ref $! value

{-# INLINE nextBit #-}
nextBit :: (Num a, Bits a) => LinearFeedbackShiftRegister a -> a -> IO a
nextBit (LinearFeedbackShiftRegister ref) mask = do
  register <- readIORef ref
  let shifted = register `unsafeShiftR` 1
  let register' =
        (shifted .&. complement mask) .|. (mask .&. negate (1 .&. (register `xor` shifted)))
  writeIORef ref $! register'
  pure register'

-- | A port is like an IORef but with a custom handler for writes.
data Port a = Port {
    portValue     :: !(IORef a)
  , portReadMask  :: !a
  , portWriteMask :: !a
  , portNotify    :: !(a -> a -> IO a)
}

-- | Create a new port.
newPort
  :: Num a
  => a                  -- ^ Initial value.
  -> a                  -- ^ Write mask.  1 indicates that the bit is writable.
  -> (a -> a -> IO a)   -- ^ Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  -> IO (Port a)
newPort value0 portWriteMask portNotify = do
  portValue <- newIORef value0
  let portReadMask = 0x00
  pure Port { .. }

-- | Create a new port.
newPortWithReadMask
  :: a                  -- ^ Initial value.
  -> a                  -- ^ Read mask.  1 indicates that the bit will always read as 1.
  -> a                  -- ^ Write mask.  1 indicates that the bit is writable.
  -> (a -> a -> IO a)   -- ^ Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  -> IO (Port a)
newPortWithReadMask value0 portReadMask portWriteMask portNotify = do
  portValue <- newIORef value0
  pure Port { .. }

alwaysUpdate :: Applicative f => a -> b -> f b
alwaysUpdate _ = pure

neverUpdate :: Applicative f => a -> b -> f a
neverUpdate = const . pure

-- | Read from the port
{-# INLINE readPort #-}
readPort :: Bits a => Port a -> IO a
readPort Port {..} = do
  value <- readIORef portValue
  pure (value .|. portReadMask)

-- | Read from the port directly skipping the read mask.
{-# INLINE directReadPort #-}
directReadPort :: Port a -> IO a
directReadPort Port {..} = readIORef portValue

-- | Write to the port and notify any listeners.
{-# INLINE writePort #-}
writePort :: Bits a => Port a -> a -> IO ()
writePort Port {..} newValue = do
  oldValue  <- readIORef portValue
  newValue' <- portNotify oldValue
                          ((oldValue .&. complement portWriteMask) .|. newValue .&. portWriteMask)
  writeIORef portValue $! newValue'

-- | Write the value of the port directly without any checks or notifications.
{-# INLINE directWritePort #-}
directWritePort :: Port a -> a -> IO ()
directWritePort Port {..} = writeIORef portValue

-- | Set writable bits and notify all listeners.
{-# INLINE setPortBits #-}
setPortBits :: Bits a => Port a -> a -> IO ()
setPortBits Port {..} newValue = do
  oldValue  <- readIORef portValue
  newValue' <- portNotify oldValue (oldValue .|. (newValue .&. portWriteMask))
  writeIORef portValue $! newValue'

-- | Set writable bits and notify all listeners.
{-# INLINE clearPortBits #-}
clearPortBits :: Bits a => Port a -> a -> IO ()
clearPortBits Port {..} newValue = do
  oldValue  <- readIORef portValue
  newValue' <- portNotify oldValue (oldValue .&. complement (newValue .&. portWriteMask))
  writeIORef portValue $! newValue'
