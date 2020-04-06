{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.CPURegisters
  ( cpuRegisters
  )
where

import           Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

cpuRegisters :: BB.Builder
cpuRegisters = table
  "cpu"
  [tr [th 1 ["CPU Registers"]]]
  [ tr [td ["AF ", value [field "rA" "00", field "rF" "00"]]]
  , tr [td ["BC ", value [field "rB" "00", field "rC" "00"]]]
  , tr [td ["DE ", value [field "rD" "00", field "rE" "00"]]]
  , tr [td ["HL ", value [field "rH" "00", field "rL" "00"]]]
  , tr [td ["SP ", value [field "rSPH" "00", field "rSPL" "00"]]]
  , tr [td ["PC ", value [field "rPCH" "00", field "rPCL" "00"]]]
  , tr
    [ td
        [ "Flags"
        , value
          [ field "i" (desc "i" "Master Interrupt Enable" [])
          , field "z" (desc "z" "Zero Flag" [])
          , field "n" (desc "n" "Negative Flag" [])
          , field "h" (desc "h" "Half Carry Flag" [])
          , field "c" (desc "c" "Carry Flag" [])
          ]
        ]
    ]
  , tr [td ["Mode", value [field "cpuMode" "Paused"]]]
  ]
