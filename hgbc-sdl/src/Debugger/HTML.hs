{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML
  ( debugHTML
  )
where

import           Data.String
import           Debugger.HTML.CPURegisters
import           Debugger.HTML.Elements
import           Debugger.HTML.LCDRegisters
import           Debugger.HTML.Memory
import           Debugger.HTML.SoundRegisters
import           Debugger.HTML.SystemRegisters
import           Prelude                 hiding ( head
                                                , div
                                                )
import qualified Data.ByteString.Builder       as BB
import qualified Network.HTTP.Types            as HTTP

debugHTML :: FilePath -> BB.Builder
debugHTML romFileName = html [header, main]
 where
  header = head
    [ title (fromString romFileName <> " - hgbc debugger")
    , charset "UTF-8"
    , meta "application-name" "hgbc debugger"
    , link "stylesheet" "css"
    , script "js"
    ]

  main = body
    [ iframe "invisible_frame"
    , nav
      [ form
          HTTP.methodPost
          "/"
          "invisible_frame"
          [ button "run"      "Run/Pause"                     [img "svg/run"]
          , button "step"     "Step (step into procedures)"   [img "svg/step"]
          , button "stepOver" "Step (execute procedures)"     [img "svg/stepthrough"]
          , button "stepOut"  "Step out of current procedure" [img "svg/stepout"]
          , button "restart"  "Restart emulator"              [img "svg/reset"]
          ]
      ]
    , divclassid
      "rootContainer"
      []
      [ div
        [ divclass "sidebyside" [cpuRegisters, memory]
        , systemRegisters
        , lcdRegisters
        , soundRegisters
        ]
      , div [disassembly]
      ]
    ]

disassembly :: BB.Builder
disassembly = divclass
  "disassembly"
  [ h 4 "Disassembly"
  , nav [input "text" "disassemblyAddress" 9 "0000:0000"]
  , divclass "window" [ulid "disassemblyList" []]
  ]
