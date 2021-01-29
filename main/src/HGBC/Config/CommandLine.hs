module HGBC.Config.CommandLine
  ( Options (..),
    parse,
  )
where

import qualified Data.Text as T
import HGBC.Config.Decode (decodeColorCorrection, decodeMode)
import qualified Machine.GBC.Color as Color
import Machine.GBC.Mode (EmulatorMode)
import Options.Applicative (Parser, ParserInfo, action, auto, completeWith, eitherReader, execParser, fullDesc, header, help, helper, info, long, metavar, option, str, strArgument, switch, value, (<**>))

data Options = Options
  { debugMode :: Bool,
    debugPort :: Maybe Int,
    noSound :: Bool,
    noVsync :: Bool,
    stats :: Bool,
    scale :: Maybe Int,
    speed :: Maybe Double,
    colorCorrection :: Maybe Color.CorrectionMode,
    bootROM :: Maybe FilePath,
    mode :: Maybe EmulatorMode,
    filename :: FilePath
  }

parse :: IO Options
parse = execParser description

description :: ParserInfo Options
description =
  info
    (commandLine <**> helper)
    (fullDesc <> header "An emulator and debugger for the Gameboy Color")

commandLine :: Parser Options
commandLine =
  Options
    <$> switch (long "debug" <> help "Enable the debugger")
    <*> option
      (Just <$> auto)
      ( long "debug-port" <> value Nothing <> metavar "DEBUG_PORT"
          <> help
            "Port to run the debug server on"
      )
    <*> switch (long "no-sound" <> help "Disable audio output")
    <*> switch (long "no-vsync" <> help "Disable vertical retrace syncrhonization")
    <*> switch (long "stats" <> help "Show performance information on stdout")
    <*> option
      (Just <$> auto)
      ( long "scale" <> value Nothing <> metavar "SCALE"
          <> help
            "Default scale factor for video output"
      )
    <*> option
      (Just <$> auto)
      ( long "speed" <> value Nothing <> metavar "SPEED"
          <> help
            "Speed as a fraction of normal speed"
      )
    <*> option
      (Just <$> eitherReader (decodeColorCorrection . T.pack))
      ( long "color-correction"
          <> value Nothing
          <> metavar "CORRECTION_MODE"
          <> completeWith ["none", "default"]
          <> help "Color correction mode. Recognized values are 'none' and 'default'"
      )
    <*> option
      (Just <$> str)
      ( long "boot-rom" <> value Nothing <> metavar "BOOT_FILE" <> action "file"
          <> help
            "Use an optional boot ROM"
      )
    <*> option
      (eitherReader (decodeMode . T.pack))
      ( long "mode"
          <> value Nothing
          <> metavar "MODE"
          <> completeWith ["dmg", "cgb", "auto"]
          <> help "Graphics mode at startup. Can be 'dmg', 'cgb', or 'auto' (default)."
      )
    <*> strArgument (metavar "ROM_FILE" <> action "file" <> help "The ROM file to run")
