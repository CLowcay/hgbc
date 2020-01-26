{-# LANGUAGE RecordWildCards #-}
module GBC.MBC.RTC
  ( savedRTC
  )
where

import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Common
import           Data.IORef
import           Data.Int
import           Data.Time.Clock.System
import           Data.Word
import           GBC.MBC.Interface
import           System.Directory
import           Text.Read

flagHalt :: Word8
flagHalt = 0x40

data RTCState = RTCState {
    seconds  :: Word8
  , minutes  :: Word8
  , hours    :: Word8
  , daysLow  :: Word8
  , daysHigh :: Word8
} deriving (Eq, Ord, Show)

makeRTC :: Int64 -> SystemTime -> RTCState
makeRTC base time =
  let stamp                = (systemSeconds time) - base
      (noSeconds, seconds) = fromIntegral <$> stamp `divMod` 60
      (noMinutes, minutes) = fromIntegral <$> noSeconds `divMod` 60
      (days     , hours  ) = fromIntegral <$> noMinutes `divMod` 24
      daysLow              = fromIntegral days .&. 0xFF
      daysHigh             = (if days >= 256 then 1 else 0) .|. (if days >= 512 then 0x80 else 0)
  in  RTCState { .. }

rtcBase :: RTCState -> SystemTime -> Int64
rtcBase RTCState {..} now = systemSeconds now - rtcAsSeconds
 where
  rtcAsSeconds =
    fromIntegral seconds
      + (60 * fromIntegral minutes)
      + (3600 * fromIntegral hours)
      + (86400 * fromIntegral daysLow)
      + (22118400 * (fromIntegral daysHigh .&. 1))

readBase :: FilePath -> IO Int64
readBase path = do
  exists <- doesFileExist path
  unless exists $ writeBase path 0
  mBase <- readMaybe <$> readFile path
  case mBase of
    Just base -> pure base
    Nothing   -> do
      putStrLn ("WARNING: " <> path <> " is not a valid hgbc rtc file")
      writeBase path 0
      pure 0

writeBase :: FilePath -> Int64 -> IO ()
writeBase path i = do
  err <- try (writeFile path (show i))
  case err of
    Left  e  -> putStrLn (displayException (e :: IOException))
    Right () -> pure ()

savedRTC :: FilePath -> IO RTC
savedRTC rtcFile = do
  isLatchedRTC <- newIORef False
  latchedRTC   <- newIORef (RTCState 0 0 0 0 0)
  rtcBaseRef   <- (newIORef $!) =<< readBase rtcFile
  haltRef      <- newIORef Nothing

  let getCurrentTime = do
        halt <- readIORef haltRef
        maybe getSystemTime pure halt

  let checkHaltMode register value = when (register == 0x0C) $ if isFlagSet flagHalt value
        then do
          halt <- readIORef haltRef
          case halt of
            Nothing -> writeIORef haltRef . Just =<< getSystemTime
            Just _  -> pure ()
        else writeIORef haltRef Nothing

  let readRTC register = do
        isLatched <- readIORef isLatchedRTC
        base      <- readIORef rtcBaseRef
        rtc       <- if isLatched then readIORef latchedRTC else makeRTC base <$> getCurrentTime
        pure $ case register of
          0x8 -> seconds rtc
          0x9 -> minutes rtc
          0xA -> hours rtc
          0xB -> daysLow rtc
          0xC -> daysHigh rtc
          x   -> error ("Read from invalid RTC register " <> show x)

  let writeRTC register value = do
        checkHaltMode register value
        now  <- getCurrentTime
        base <- readIORef rtcBaseRef
        let rtc = makeRTC base now
        let rtc' = case register of
              0x8 -> rtc { seconds = fromIntegral value }
              0x9 -> rtc { minutes = fromIntegral value }
              0xA -> rtc { hours = fromIntegral value }
              0xB -> rtc { daysLow = fromIntegral value }
              0xC -> rtc { daysHigh = fromIntegral value .&. 1 }
              x   -> error ("Write to invalid RTC register " <> show x)
        let base' = rtcBase rtc' now
        writeIORef rtcBaseRef base'
        writeBase rtcFile base'

  let latchRTC value = do
        when (value == 0) $ writeIORef isLatchedRTC False
        when (value == 1) $ do
          isLatched <- readIORef isLatchedRTC
          unless isLatched $ do
            writeIORef isLatchedRTC True
            base <- readIORef rtcBaseRef
            writeIORef latchedRTC . makeRTC base =<< getSystemTime

  pure RTC { .. }
