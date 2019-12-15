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
import           Data.Functor
import           Data.IORef
import           Data.Word
import           GBC.Audio.Common
import           GBC.Audio.Length
import           GBC.Primitive
import qualified Data.Vector                   as V

data WaveChannel = WaveChannel {
    output           :: !(IORef Int)
  , enable           :: !(IORef Bool)
  , port0            :: !(Port Word8)
  , port1            :: !(Port Word8)
  , port2            :: !(Port Word8)
  , port3            :: !(Port Word8)
  , port4            :: !(Port Word8)
  , port52           :: !(Port Word8)
  , portWaveTable    :: !(V.Vector (Port Word8))
  , frequencyCounter :: !Counter
  , sample           :: !(StateCycle Int)
  , lengthCounter    :: !Length
}

waveSamplerStates :: [(Int, Int)]
waveSamplerStates = [0 .. 31] <&> (, 1)

newWaveChannel :: Port Word8 -> IO WaveChannel
newWaveChannel port52 = mdo
  output <- newIORef 0
  enable <- newIORef False

  port0  <- newPortWithReadMask 0x00 0x7F 0x80 $ \_ register0 -> do
    unless (isFlagSet flagMasterEnable register0) $ disableIO port52 output enable
    pure register0

  port1 <- newPortWithReadMask 0xFF 0xFF 0xFF $ \_ register1 -> do
    register4 <- directReadPort port4
    when (isFlagSet flagLength register4) $ reloadLength lengthCounter register1
    pure register1

  port2 <- newPortWithReadMask 0xFF 0x9F 0x60 alwaysUpdate

  port3 <- newPortWithReadMask 0xFF 0xFF 0xFF $ \_ register3 -> do
    register4 <- directReadPort port4
    reloadCounter frequencyCounter (getTimerPeriod (getFrequency register3 register4))
    pure register3

  port4 <- newPortWithReadMask 0xFF 0xBF 0xC7 $ \_ register4 -> do
    when (isFlagSet flagTrigger register4) $ do
      register0 <- directReadPort port0
      register3 <- directReadPort port3
      initLength lengthCounter
      reloadCounter frequencyCounter (getTimerPeriod (getFrequency register3 register4))
      resetStateCycle sample waveSamplerStates
      let enabled = isFlagSet flagMasterEnable register0
      writeIORef enable enabled
      updateStatus port52 flagChannel3Enable enabled
    pure register4

  portWaveTable    <- V.replicateM 16 (newPort 0x00 0xFF alwaysUpdate)

  frequencyCounter <- newCounter
  sample           <- newStateCycle waveSamplerStates
  lengthCounter    <- newLength 0xFF
  pure WaveChannel { .. }

disableIO :: Port Word8 -> IORef Int -> IORef Bool -> IO ()
disableIO port52 output enable = do
  writeIORef output 0
  writeIORef enable False
  updateStatus port52 flagChannel3Enable False

instance Channel WaveChannel where
  getOutput WaveChannel {..} = readIORef output
  disable WaveChannel {..} = disableIO port52 output enable
  getStatus WaveChannel {..} = readIORef enable
  getPorts WaveChannel {..} =
    [(0, port0), (1, port1), (2, port2), (3, port3), (4, port4)]
      ++ ([22 ..] `zip` toList portWaveTable)

  frameSequencerClock WaveChannel {..} FrameSequencerOutput {..} = do
    register4 <- directReadPort port4
    when (lengthClock && isFlagSet flagLength register4)
      $ clockLength lengthCounter (disableIO port52 output enable)

  masterClock WaveChannel {..} clockAdvance = do
    isEnabled <- readIORef enable
    when isEnabled $ updateCounter frequencyCounter clockAdvance $ do
      void $ updateStateCycle sample 1 $ \i -> do
        sampleByte <- directReadPort (portWaveTable V.! (i `unsafeShiftR` 1))
        register2  <- directReadPort port2
        let volume = getVolume register2
        let rawSampleValue =
              if i .&. 1 == 0 then sampleByte `unsafeShiftR` 4 else sampleByte .&. 0x0F
        let sampleValue =
              if volume == 0 then 0 else rawSampleValue `unsafeShiftR` (fromIntegral volume - 1)
        writeIORef output (fromIntegral sampleValue - 8)
      register3 <- directReadPort port3
      register4 <- directReadPort port4
      pure (getTimerPeriod (getFrequency register3 register4))

flagMasterEnable :: Word8
flagMasterEnable = 0x80

getVolume :: Word8 -> Word8
getVolume register2 = 3 .&. (register2 `unsafeShiftR` 5)

getTimerPeriod :: Int -> Int
getTimerPeriod f = (2 * (2048 - f)) - 1
