{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML
  ( debugHTML
  )
where

import           Data.String
import           Debugger.HTML.CPURegisters
import           Debugger.HTML.Elements
import           Debugger.HTML.LCDRegisters
import           Debugger.HTML.SoundRegisters
import           Debugger.HTML.SystemRegisters
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
      [div [disassembly, memory], div [cpuRegisters, systemRegisters, lcdRegisters, soundRegisters]]
    ]

disassembly :: BB.Builder
disassembly = divclass
  "'disassembly panel'"
  [ innerNav
    [ input "text" "disassemblyAddress" 9 "0000:0000"
    , button "runTo" "Run to current address" [img "svg/runto"]
    , button "toPC"  "Go to current PC"       [img "svg/home"]
    ]
    [ button "breakpoint" "Toggle breakpoint" [img "svg/breakpoint"]
    , button "label"      "Create label"      [img "svg/label"]
    ]
  , focusDiv "window" [ulid "disassemblyList" []]
  ]

memory :: BB.Builder
memory = divclass
  "'memory panel'"
  [ nav [input "text" "address" 9 "0000"]
  , focusDiv
    "window"
    [ divclassid "addressLabels" ["addresses"] []
    , divclassid "memoryHex"     ["hex"]       []
    , divclassid "memoryASCII"   ["ascii"]     []
    ]
  ]
