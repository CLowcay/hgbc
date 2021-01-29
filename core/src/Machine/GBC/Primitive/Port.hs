{-# LANGUAGE RecordWildCards #-}

module Machine.GBC.Primitive.Port
  ( Port,
    new,
    newWithReadAction,
    newWithReadMask,
    alwaysUpdate,
    neverUpdate,
    read,
    readDirect,
    write,
    writeDirect,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits (Bits (..))
import Data.Word (Word8)
import Machine.GBC.Primitive.UnboxedRef
import Prelude hiding (read)

-- | A port is like an IORef but with a custom handler for writes.
data Port = Port
  { portWriteMask :: !Word8,
    portValue :: !(UnboxedRef Word8),
    portRead :: !(Word8 -> IO Word8),
    portNotify :: !(Word8 -> Word8 -> IO Word8)
  }

-- | Create a new port.
new ::
  -- | Initial value.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
new value0 portWriteMask portNotify = do
  portValue <- newUnboxedRef value0
  let portRead = pure
  pure Port {..}

-- | Create a new port with a custom action to run when reading.
newWithReadAction ::
  -- | Initial value.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to perform on reads.
  (Word8 -> IO Word8) ->
  -- | Action to perform on writes.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newWithReadAction value0 portWriteMask portRead portNotify = do
  portValue <- newUnboxedRef value0
  pure Port {..}

-- | Create a new port.
newWithReadMask ::
  -- | Initial value.
  Word8 ->
  -- | Read mask.  1 indicates that the bit will always read as 1.
  Word8 ->
  -- | Write mask.  1 indicates that the bit is writable.
  Word8 ->
  -- | Action to handle writes.  Paramters are oldValue -> newValue -> valueToWrite.
  (Word8 -> Word8 -> IO Word8) ->
  IO Port
newWithReadMask value0 portReadMask portWriteMask portNotify = do
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
{-# INLINE read #-}
read :: MonadIO m => Port -> m Word8
read Port {..} = liftIO . portRead =<< readUnboxedRef portValue

-- | Read from the port directly skipping the read mask.
{-# INLINE readDirect #-}
readDirect :: MonadIO m => Port -> m Word8
readDirect Port {..} = readUnboxedRef portValue

-- | Write to the port and notify any listeners.
{-# INLINE write #-}
write :: MonadIO m => Port -> Word8 -> m ()
write Port {..} newValue = do
  oldValue <- readUnboxedRef portValue
  newValue' <-
    liftIO
      (portNotify oldValue ((oldValue .&. complement portWriteMask) .|. newValue .&. portWriteMask))
  writeUnboxedRef portValue newValue'

-- | Write the value of the port directly without any checks or notifications.
{-# INLINE writeDirect #-}
writeDirect :: MonadIO m => Port -> Word8 -> m ()
writeDirect Port {..} v = liftIO (writeUnboxedRef portValue v)
