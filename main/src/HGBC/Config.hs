{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module HGBC.Config
  ( Config (..),
    load,
  )
where

import Control.Monad
import qualified Data.ByteString as B
import Data.FileEmbed
import Data.Functor.Identity
import Data.Maybe
import qualified HGBC.Config.CommandLine as CommandLine
import HGBC.Config.File
import qualified HGBC.Config.Paths as Paths
import HGBC.Errors
import HGBC.Keymap
  ( Keymap,
    ScancodeDecoder,
  )
import qualified Machine.GBC.Color as Color
import qualified Machine.GBC.ROM as ROM
import System.Directory
import System.FilePath

-- | Configure HGBC.
load ::
  Ord k =>
  -- | Platform specific keycode decoder.
  ScancodeDecoder k ->
  -- | Default keymap (platform specific).
  Keymap k ->
  IO ([FileParseErrors], CommandLine.Options, Config k Identity)
load decodeScancode defaultKeymap = do
  options <- CommandLine.parse
  romPaths <- Paths.romPaths (CommandLine.filename options)
  (errors0, mainConfig) <- loadMainConfig decodeScancode
  (errors1, romConfig) <- loadROMConfig decodeScancode romPaths
  pure
    ( errors0 <> errors1,
      options,
      finalize defaultKeymap (mainConfig <> romConfig <> optionsToConfig options)
    )

-- Load the main configuration file (or create it if it doesn't exist).
loadMainConfig :: Ord k => ScancodeDecoder k -> IO ([FileParseErrors], Config k Maybe)
loadMainConfig decodeScancode = do
  baseDir <- Paths.base
  let configFile = baseDir </> "config.toml"
  exists <- doesFileExist configFile
  unless exists $ do
    createDirectoryIfMissing True baseDir
    B.writeFile configFile $(embedOneFileOf ["data/default.toml", "../data/default.toml"])

  handleConfigErrors configFile <$> parseFile decodeScancode configFile

-- | Load configuration files specific to a particular ROM.
loadROMConfig :: Ord k => ScancodeDecoder k -> ROM.Paths -> IO ([FileParseErrors], Config k Maybe)
loadROMConfig decodeScancode ROM.Paths {..} = do
  let configFile = takeDirectory romSaveFile </> "config.toml"
  exists <- doesFileExist configFile
  if exists
    then handleConfigErrors configFile <$> parseFile decodeScancode configFile
    else pure mempty

handleConfigErrors ::
  Ord k => FilePath -> Either [String] (Config k Maybe) -> ([FileParseErrors], Config k Maybe)
handleConfigErrors _ (Right config) = ([], config)
handleConfigErrors configFile (Left errors) = ([(configFile, errors)], mempty)

optionsToConfig :: Ord k => CommandLine.Options -> Config k Maybe
optionsToConfig CommandLine.Options {..} =
  mempty
    { scale = scale,
      speed = speed,
      noVsync = Just noVsync,
      debugPort = debugPort,
      bootROM = bootROM,
      colorCorrection = colorCorrection,
      mode = mode
    }

finalize :: Keymap k -> Config k Maybe -> Config k Identity
finalize defaultKeymap Config {..} =
  Config
    { speed = fromMaybe 1 speed,
      scale = fromMaybe 2 scale,
      noVsync = noVsync == Just True,
      debugPort = fromMaybe 8080 debugPort,
      bootROM = bootROM,
      colorCorrection = fromMaybe Color.DefaultCorrection colorCorrection,
      mode = mode,
      keypad = fromMaybe defaultKeymap keypad,
      backgroundPalette = fromMaybe defaultPalette backgroundPalette,
      sprite1Palette = fromMaybe defaultPalette sprite1Palette,
      sprite2Palette = fromMaybe defaultPalette sprite2Palette
    }
  where
    defaultPalette = (0x0f380fff, 0x306230ff, 0x8bac0fff, 0x9bbc0fff)
