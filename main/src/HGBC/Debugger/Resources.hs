{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HGBC.Debugger.Resources
  ( css
  , js
  , svgRun
  , svgPause
  , svgStep
  , svgStepout
  , svgStepthrough
  , svgReset
  , svgRunto
  , svgBreakpoint
  , svgBreakpointDisabled
  , svgHome
  , svgLabel
  , svgDownload
  )
where

import           Data.FileEmbed
import qualified Data.ByteString.Lazy          as LB

svgRun :: LB.ByteString
svgRun = LB.fromStrict $(embedOneFileOf ["data/play.svg", "../data/play.svg"])

svgPause :: LB.ByteString
svgPause = LB.fromStrict $(embedOneFileOf ["data/pause.svg", "../data/pause.svg"])

svgStep :: LB.ByteString
svgStep = LB.fromStrict $(embedOneFileOf ["data/step.svg", "../data/step.svg"])

svgStepout :: LB.ByteString
svgStepout = LB.fromStrict $(embedOneFileOf ["data/stepout.svg", "../data/stepout.svg"])

svgStepthrough :: LB.ByteString
svgStepthrough =
  LB.fromStrict $(embedOneFileOf ["data/stepthrough.svg", "../data/stepthrough.svg"])

svgReset :: LB.ByteString
svgReset = LB.fromStrict $(embedOneFileOf ["data/reset.svg", "../data/reset.svg"])

svgRunto :: LB.ByteString
svgRunto = LB.fromStrict $(embedOneFileOf ["data/runto.svg", "../data/runto.svg"])

svgBreakpoint :: LB.ByteString
svgBreakpoint = LB.fromStrict $(embedOneFileOf ["data/breakpoint.svg", "../data/breakpoint.svg"])

svgBreakpointDisabled :: LB.ByteString
svgBreakpointDisabled = LB.fromStrict
  $(embedOneFileOf ["data/breakpoint_disabled.svg", "../data/breakpoint_disabled.svg"])

svgHome :: LB.ByteString
svgHome = LB.fromStrict $(embedOneFileOf ["data/home.svg", "../data/home.svg"])

svgLabel :: LB.ByteString
svgLabel = LB.fromStrict $(embedOneFileOf ["data/label.svg", "../data/label.svg"])

svgDownload :: LB.ByteString
svgDownload = LB.fromStrict $(embedOneFileOf ["data/download.svg", "../data/download.svg"])

css :: LB.ByteString
css = LB.fromStrict $(embedOneFileOf ["data/debugger.css","../data/debugger.css"])

js :: LB.ByteString
js = LB.fromStrict $(embedOneFileOf ["data/debugger.js","../data/debugger.js"])
