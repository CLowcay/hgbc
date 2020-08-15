{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.HTML.CPURegisters
  ( cpuRegisters,
  )
where

import qualified Data.ByteString.Builder as BB
import HGBC.Debugger.HTML.Elements

cpuRegisters :: BB.Builder
cpuRegisters =
  table
    "'panel cpu'"
    [tr [th 1 ["CPU Registers"]]]
    [ tr [td ["AF ", value [field 2 "rA" "00", field 2 "rF" "00"]]],
      tr [td ["BC ", value [field 2 "rB" "00", field 2 "rC" "00"]]],
      tr [td ["DE ", value [field 2 "rD" "00", field 2 "rE" "00"]]],
      tr [td ["HL ", value [field 2 "rH" "00", field 2 "rL" "00"]]],
      tr [td ["SP ", value [field 2 "rSPH" "00", field 2 "rSPL" "00"]]],
      tr [td ["PC ", value [field 2 "rPCH" "00", field 2 "rPCL" "00"]]],
      tr
        [ td
            [ "Flags",
              value
                [ descField "Master Interrupt Enable" [] [field 1 "i" "i"],
                  descField "Zero Flag" [] [field 1 "z" "z"],
                  descField "Negative Flag" [] [field 1 "n" "n"],
                  descField "Half Carry Flag" [] [field 1 "h" "h"],
                  descField "Carry Flag" [] [field 1 "c" "c"]
                ]
            ]
        ],
      tr [td ["Mode", value [field 4 "cpuMode" "RUN"]]]
    ]
