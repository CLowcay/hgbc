{-# LANGUAGE RecordWildCards #-}

module HGBC.Config.Paths
  ( base,
    debugState,
    romPaths,
  )
where

import Data.Functor
import qualified Machine.GBC.ROM as ROM
import System.Directory
import System.FilePath

base :: IO FilePath
base = getAppUserDataDirectory "hgbc"

debugState :: FilePath -> IO FilePath
debugState romFile = base <&> (</> "rom" </> takeBaseName romFile)

romPaths :: FilePath -> IO ROM.Paths
romPaths romFile = do
  baseDir <- base <&> (</> "rom")
  let romDir = baseDir </> takeBaseName romFile
  let romSaveFile = romDir </> "battery"
  let romRTCFile = romDir </> "rtc"
  pure ROM.Paths {..}
