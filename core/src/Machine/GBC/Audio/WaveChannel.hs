{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Machine.GBC.Audio.WaveChannel
  ( WaveChannel,
    newWaveChannel,
    portWaveTable,
  )
where

import Control.Monad.Reader (unless, when)
import Data.Bits (Bits ((.&.)))
import Data.Foldable (Foldable (toList))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.Word (Word8)
import Machine.GBC.Audio.Common (Channel (..), FrameSequencerOutput, flagChannel3Enable, flagLength, flagTrigger, getFrequency, isLengthClockingStep, newAudioPortWithReadMask, updateStatus)
import Machine.GBC.Audio.Length (Length, clockLength, extraClocks, initLength, newLength, powerOffLength, reloadLength)
import Machine.GBC.Primitive.Counter (Counter)
import qualified Machine.GBC.Primitive.Counter as Counter
import Machine.GBC.Primitive.Port (Port)
import qualified Machine.GBC.Primitive.Port as Port
import Machine.GBC.Primitive.StateCycle (StateCycle)
import qualified Machine.GBC.Primitive.StateCycle as StateCycle
import Machine.GBC.Primitive.UnboxedRef (UnboxedRef, newUnboxedRef, readUnboxedRef, writeUnboxedRef)
import Machine.GBC.Util (isFlagSet, (.>>.))

data WaveChannel = WaveChannel
  { output :: !(UnboxedRef Int),
    enable :: !(IORef Bool),
    port0 :: !Port,
    port1 :: !Port,
    port2 :: !Port,
    port3 :: !Port,
    port4 :: !Port,
    port52 :: !Port,
    portWaveTable :: !(V.Vector Port),
    frequencyCounter :: !Counter,
    sample :: !(UnboxedRef Int),
    sampleBuffer :: !(UnboxedRef Word8),
    lengthCounter :: !Length
  }

newWaveChannel :: Port -> StateCycle FrameSequencerOutput -> IO WaveChannel
newWaveChannel port52 frameSequencer = mdo
  output <- newUnboxedRef 0
  enable <- newIORef False

  port0 <- newAudioPortWithReadMask port52 0x00 0x7F 0x80 $ \_ register0 -> do
    unless (isFlagSet flagMasterEnable register0) $ disableIO port52 output enable
    pure register0

  port1 <- newAudioPortWithReadMask port52 0xFF 0xFF 0xFF $ \_ register1 -> do
    reloadLength lengthCounter register1
    pure register1

  port2 <- newAudioPortWithReadMask port52 0xFF 0x9F 0x60 Port.alwaysUpdate

  port3 <- newAudioPortWithReadMask port52 0xFF 0xFF 0xFF Port.alwaysUpdate

  port4 <- newAudioPortWithReadMask port52 0xFF 0xBF 0xC7 $ \previous register4 -> do
    frame <- StateCycle.getState frameSequencer
    when
      (isFlagSet flagLength register4 && not (isFlagSet flagLength previous))
      (extraClocks lengthCounter frame (disableIO port52 output enable))

    when (isFlagSet flagTrigger register4) $ do
      register0 <- Port.readDirect port0
      register3 <- Port.readDirect port3
      initLength lengthCounter frame (isFlagSet flagLength register4)
      Counter.reload frequencyCounter (6 + getTimerPeriod (getFrequency register3 register4))
      writeUnboxedRef sample 0
      let enabled = isFlagSet flagMasterEnable register0
      writeIORef enable enabled
      updateStatus port52 flagChannel3Enable enabled
      sampleByte <- readUnboxedRef sampleBuffer
      generateOutput channel sampleByte 0
    pure register4

  let readWaveMemory v = do
        isEnabled <- readIORef enable
        if not isEnabled
          then pure v
          else do
            i <- readUnboxedRef sample
            Port.readDirect (portWaveTable V.! (i .>>. 1))

  let writeWaveMemory old v = do
        isEnabled <- readIORef enable
        if not isEnabled
          then pure v
          else do
            i <- readUnboxedRef sample
            Port.writeDirect (portWaveTable V.! (i .>>. 1)) v
            pure old

  portWaveTable <- V.replicateM 16 (Port.newWithReadAction 0x00 0xFF readWaveMemory writeWaveMemory)

  frequencyCounter <- Counter.new 0x7FF
  sample <- newUnboxedRef 0
  sampleBuffer <- newUnboxedRef 0
  lengthCounter <- newLength 0xFF
  let channel = WaveChannel {..}
  pure channel

disableIO :: Port -> UnboxedRef Int -> IORef Bool -> IO ()
disableIO port52 output enable = do
  writeUnboxedRef output 0
  writeIORef enable False
  updateStatus port52 flagChannel3Enable False

instance Channel WaveChannel where
  getOutput WaveChannel {..} = readUnboxedRef output
  disable WaveChannel {..} = disableIO port52 output enable
  getStatus WaveChannel {..} = readIORef enable
  getPorts WaveChannel {..} =
    [(0, port0), (1, port1), (2, port2), (3, port3), (4, port4)]
      ++ ([22 ..] `zip` toList portWaveTable)

  powerOff WaveChannel {..} = do
    Port.writeDirect port0 0
    Port.writeDirect port1 0
    Port.writeDirect port2 0
    Port.writeDirect port3 0
    Port.writeDirect port4 0
    powerOffLength lengthCounter
    writeUnboxedRef sampleBuffer 0

  frameSequencerClock WaveChannel {..} step = do
    register4 <- Port.readDirect port4
    when (isLengthClockingStep step && isFlagSet flagLength register4) $
      clockLength lengthCounter (disableIO port52 output enable)

  masterClock channel@WaveChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $
      Counter.update frequencyCounter clockAdvance $ do
        i0 <- readUnboxedRef sample
        let i = (i0 + 1) .&. 0x1F
        writeUnboxedRef sample i
        sampleByte <- Port.readDirect (portWaveTable V.! (i .>>. 1))
        writeUnboxedRef sampleBuffer sampleByte
        generateOutput channel sampleByte i

        register3 <- Port.readDirect port3
        register4 <- Port.readDirect port4
        pure (getTimerPeriod (getFrequency register3 register4))

  directReadPorts WaveChannel {..} =
    (,,,,)
      <$> Port.readDirect port0
      <*> Port.readDirect port1
      <*> Port.readDirect port2
      <*> Port.readDirect port3
      <*> Port.readDirect port4

{-# INLINE generateOutput #-}
generateOutput :: WaveChannel -> Word8 -> Int -> IO ()
generateOutput WaveChannel {..} sampleByte i = do
  register2 <- Port.readDirect port2
  let volume = getVolume register2
  let rawSampleValue = if i .&. 1 == 0 then sampleByte .>>. 4 else sampleByte .&. 0x0F
  let sampleValue = if volume == 0 then 0 else rawSampleValue .>>. (fromIntegral volume - 1)
  writeUnboxedRef output (fromIntegral sampleValue)

flagMasterEnable :: Word8
flagMasterEnable = 0x80

getVolume :: Word8 -> Word8
getVolume register2 = 3 .&. (register2 .>>. 5)

getTimerPeriod :: Int -> Int
getTimerPeriod f = 2 * (2048 - f)
