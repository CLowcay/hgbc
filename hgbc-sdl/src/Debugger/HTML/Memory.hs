{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.Memory
  ( memory
  )
where

import           Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

memory :: BB.Builder
memory = divclass
  "'panel memory'"
  [ divclass "addressContainer" [input "text" "address" 9 "0000"]
  , h 4 "Memory"
  , divclassid "addressLabels" ["addresses"] ["0000"]
  , divclassid "memoryHex"     ["hex"]       ["00 00 00 00 00 00 00 00"]
  , divclassid "memoryASCII"   ["ascii"]     ["........"]
  ]
