{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module GBC.Audio.WaveChannel
  ( WaveChannel
  , newWaveChannel
  )
where

import           Common
import           Control.Monad.Reader
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Audio.Length
import           GBC.Primitive
import           GBC.Primitive.UnboxedRef
import qualified Data.Vector                   as V

data WaveChannel = WaveChannel {
    output           :: !(UnboxedRef Int)
  , enable           :: !(IORef Bool)
  , port0            :: !(Port Word8)
  , port1            :: !(Port Word8)
  , port2            :: !(Port Word8)
  , port3            :: !(Port Word8)
  , port4            :: !(Port Word8)
  , port52           :: !(Port Word8)
  , portWaveTable    :: !(V.Vector (Port Word8))
  , frequencyCounter :: !Counter
  , sample           :: !(UnboxedRef Int)
  , sampleBuffer     :: !(UnboxedRef Word8)
  , lengthCounter    :: !Length
}

newWaveChannel :: Port Word8 -> IO WaveChannel
newWaveChannel port52 = mdo
  output <- newUnboxedRef 0
  enable <- newIORef False

  port0  <- newAudioPortWithReadMask port52 0x00 0x7F 0x80 $ \_ register0 -> do
    unless (isFlagSet flagMasterEnable register0) $ disableIO port52 output enable
    pure register0

  port1 <- newAudioPortWithReadMask port52 0xFF 0xFF 0xFF $ \_ register1 -> do
    reloadLength lengthCounter register1
    pure register1

  port2 <- newAudioPortWithReadMask port52 0xFF 0x9F 0x60 alwaysUpdate

  port3 <- newAudioPortWithReadMask port52 0xFF 0xFF 0xFF alwaysUpdate

  port4 <- newAudioPortWithReadMask port52 0xFF 0xBF 0xC7 $ \previous register4 -> do
    when (isFlagSet flagLength register4 && not (isFlagSet flagLength previous))
         (extraClocks lengthCounter (disableIO port52 output enable))

    when (isFlagSet flagTrigger register4) $ do
      register0 <- directReadPort port0
      register3 <- directReadPort port3
      initLength lengthCounter (isFlagSet flagLength register4)
      reloadCounter frequencyCounter (6 + getTimerPeriod (getFrequency register3 register4))
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
            directReadPort (portWaveTable V.! (i .>>. 1))

  let writeWaveMemory old v = do
        isEnabled <- readIORef enable
        if not isEnabled
          then pure v
          else do
            i <- readUnboxedRef sample
            directWritePort (portWaveTable V.! (i .>>. 1)) v
            pure old

  portWaveTable <- V.replicateM 16 (newPortWithReadAction 0x00 0xFF readWaveMemory writeWaveMemory)

  frequencyCounter <- newCounter 0x7FF
  sample           <- newUnboxedRef 0
  sampleBuffer     <- newUnboxedRef 0
  lengthCounter    <- newLength 0xFF
  let channel = WaveChannel { .. }
  pure channel

disableIO :: Port Word8 -> UnboxedRef Int -> IORef Bool -> IO ()
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
    directWritePort port0 0
    directWritePort port1 0
    directWritePort port2 0
    directWritePort port3 0
    directWritePort port4 0
    powerOffLength lengthCounter
    writeUnboxedRef sampleBuffer 0

  frameSequencerClock WaveChannel {..} FrameSequencerOutput {..} = do
    register4 <- directReadPort port4
    clockLength lengthCounter
                lengthClock
                (isFlagSet flagLength register4)
                (disableIO port52 output enable)

  masterClock channel@WaveChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $ do
      reloads <- updateReloadingCounter frequencyCounter clockAdvance $ do
        register3 <- directReadPort port3
        register4 <- directReadPort port4
        pure (getTimerPeriod (getFrequency register3 register4))

      when (reloads > 0) $ do
        i0 <- readUnboxedRef sample
        let i = (i0 + reloads) .&. 0x1F
        writeUnboxedRef sample i
        sampleByte <- directReadPort (portWaveTable V.! (i .>>. 1))
        writeUnboxedRef sampleBuffer sampleByte
        generateOutput channel sampleByte i

generateOutput :: WaveChannel -> Word8 -> Int -> IO ()
generateOutput WaveChannel {..} sampleByte i = do
  register2 <- directReadPort port2
  let volume         = getVolume register2
  let rawSampleValue = if i .&. 1 == 0 then sampleByte .>>. 4 else sampleByte .&. 0x0F
  let sampleValue = if volume == 0 then 0 else rawSampleValue .>>. (fromIntegral volume - 1)
  writeUnboxedRef output (fromIntegral sampleValue - 8)

flagMasterEnable :: Word8
flagMasterEnable = 0x80

getVolume :: Word8 -> Word8
getVolume register2 = 3 .&. (register2 .>>. 5)

getTimerPeriod :: Int -> Int
getTimerPeriod f = 2 * (2048 - f)
