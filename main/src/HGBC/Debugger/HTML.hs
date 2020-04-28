{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.HTML
  ( debugHTML
  )
where

import           Data.String
import           HGBC.Debugger.HTML.CPURegisters
import           HGBC.Debugger.HTML.Elements
import           HGBC.Debugger.HTML.LCDRegisters
import           HGBC.Debugger.HTML.SoundRegisters
import           HGBC.Debugger.HTML.SystemRegisters
import           Prelude                 hiding ( head
                                                , div
                                                )
import qualified Data.ByteString.Builder       as BB

debugHTML :: FilePath -> Int -> BB.Builder
debugHTML romFileName bootROMLimit = html [header, main]
 where
  header = head
    [ title (fromString romFileName <> " - hgbc debugger")
    , charset "UTF-8"
    , meta "application-name" "hgbc debugger"
    , link "stylesheet" "css"
    , inlineScript ("window.bootROMLimit = " <> BB.intDec bootROMLimit)
    , script "js"
    ]

  main = body
    [ nav
      [ button "run"      "Run / Pause" [img "svg/run", "Run"]
      , button "step"     "Step in"     [img "svg/step", "Step in"]
      , button "stepOver" "Step"        [img "svg/stepthrough", "Step"]
      , button "stepOut"  "Step out"    [img "svg/stepout", "Step out"]
      , button "restart"  "Restart"     [img "svg/reset", "Restart"]
      ]
    , divclassid
      "rootContainer"
      []
      [ div [disassembly, divclass "sideBySide" [memory, backtrace, stack]]
      , div
        [ divclass "sideBySide" [cpuRegisters, listViews]
        , systemRegisters
        , lcdRegisters
        , soundRegisters
        ]
      ]
    ]

disassembly :: BB.Builder
disassembly = divclass
  "'disassembly panel'"
  [ innerNav
    [ input "text" "disassemblyAddress" 5 "0000:0000"
    , button "toPC"  "Go to current PC"       [img "svg/home"]
    , button "runTo" "Run to current address" [img "svg/runto"]
    ]
    [ button "breakpoint"          "Toggle breakpoint"    [img "svg/breakpoint"]
    , button "label"               "Create label"         [img "svg/label"]
    , button "downloadDisassembly" "Download disassembly" [img "svg/download"]
    ]
  , focusDiv "window" [ulid "disassemblyList" []]
  ]

memory :: BB.Builder
memory = divclass
  "'memory panel'"
  [ innerNav
    [input "text" "address" 5 "0000"]
    [ button "align"  "Align to nearest 8 byte boundary" ["Align"]
    , button "gotoHL" "Scroll to (HL)"                   ["(HL)"]
    , button "gotoBC" "Scroll to (BC)"                   ["(BC)"]
    , button "gotoDE" "Scroll to (DE)"                   ["(DE)"]
    , button "gotoC"  "Scroll to $FF00 + (C)"            ["(C)"]
    ]
  , focusDiv
    "window"
    [ divclassid "addressLabels" ["addresses"] []
    , divclassid "memoryHex"     ["hex"]       []
    , divclassid "memoryASCII"   ["ascii"]     []
    ]
  ]

listViews :: BB.Builder
listViews = tabs
  "list-views"
  "panel"
  [ ("Labels"      , [divclass "autoWindow" [ulid "labels-list" []]])
  , ("Break points", [divclass "autoWindow" [ulid "breakpoints-list" []]])
  ]

stack :: BB.Builder
stack = divclassid
  "stackPanel"
  ["panel"]
  [ button "gotoSP" "Go to current SP" [img "svg/home"]
  , h 4 "Stack"
  , focusDiv "window" [ulid "stack" []]
  ]

backtrace :: BB.Builder
backtrace = divclassid
  "backtracePanel"
  ["panel"]
  [h 4 "Backtrace", divclass "'window backtraceWindow'" [ulid "backtrace" []]]
