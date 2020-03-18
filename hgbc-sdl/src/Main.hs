{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  )
where

import           Control.Monad.Except
import           Control.Monad.Writer.Lazy
import           Data.Foldable
import           Data.Functor
import           Machine.GBC
import           Options.Applicative
import           System.Directory
import           System.FilePath
import qualified Data.ByteString               as B

data Options = Options
  { debugMode :: Bool
  , filename  :: FilePath
  }

optionsP :: Parser Options
optionsP = Options <$> switch (long "debug" <> help "Enable the debugger") <*> strArgument
  (metavar "ROM-FILE" <> help "The ROM file to run")

description :: ParserInfo Options
description =
  info (optionsP <**> helper) (fullDesc <> header "hgbc-sdl - a Gameboy Color emulator")

getROMPaths :: FilePath -> IO ROMPaths
getROMPaths romFile = do
  baseDir <- getAppUserDataDirectory "hgbc" <&> (</> "rom")
  createDirectoryIfMissing True (baseDir </> takeBaseName romFile)
  let romSaveFile = baseDir </> romFile </> "battery"
  let romRTCFile  = baseDir </> romFile </> "rtc"
  pure ROMPaths { .. }

main :: IO ()
main = do
  Options {..} <- execParser description
  fileContent  <- B.readFile filename
  romPaths     <- getROMPaths filename
  let (eROM, warnings) = runWriter (runExceptT (parseROM romPaths fileContent))

  unless (null warnings) $ do
    putStrLn ("Some problems were detected in " <> filename <> ":")
    for_ warnings $ \message -> putStrLn (" - " <> message)

  case eROM of
    Left err -> do
      putStrLn ("Cannot load " <> filename <> " because:")
      putStrLn (" - " <> err)
    Right rom -> putStrLn "hello"
