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
          [ button "run"      [img "svg/run", "Run"]
          , button "step"     [img "svg/step", "Step in"]
          , button "stepOver" [img "svg/stepthrough", "Step"]
          , button "stepOut"  [img "svg/stepout", "Step out"]
          , button "restart"  [img "svg/reset", "Restart"]
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
  "'panel disassembly'"
  [ h 4 "Disassembly"
  , nav [input "text" "disassemblyAddress" 9 "0000:0000", button "toPC" ["Scroll to PC"]]
  , divclass "window" [ulid "disassemblyList" []]
  ]
