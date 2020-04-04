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
import           Prelude                 hiding ( head )
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
          [ button "run"      "Run"
          , button "step"     "Step"
          , button "stepOver" "Step Through"
          , button "stepOut"  "Step Out"
          , button "restart"  "Restart"
          ]
      ]
    , divclassid
      "rootContainer"
      []
      [ divclassid
          "leftContainer"
          []
          [ divclass "sidebyside" [cpuRegisters, memory]
          , systemRegisters
          , lcdRegisters
          , soundRegisters
          ]
      ]
    ]
