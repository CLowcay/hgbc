{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.SystemRegisters
  ( systemRegisters
  )
where

import           Data.Functor
import           Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

systemRegisters :: BB.Builder
systemRegisters = table
  "'panel hardware'"
  [tr [th 5 ["System Registers"]]]
  [ tr
    [ td
      [ "KEY1"
      , br
      , value
        (  descField "CPU Speed"
                     [ul ["0: Normal speed (1MiHz)", "1: Double speed (2MiHz)"]]
                     [field 1 "key17" "0"]

        :  replicate 6 unused
        <> [ descField "Speed Switch"
                       [ul ["0: Switch to normal speed", "1: Switch to double speed"]]
                       [field 1 "key10" "0"]
           ]
        )
      ]
    , td
      [ "VBK"
      , br
      , value
        (  replicate 7 unused
        <> [descField "VRAM Bank" [p "Video RAM bank number"] [field 1 "vbk0" "0"]]
        )
      ]
    , td
      [ "SVBK"
      , br
      , value
        [ field 1 "svbk7" "0"
        , unused
        , field 1 "svbk5" "0"
        , field 1 "svbk4" "0"
        , field 1 "svbk3" "0"
        , descField "RAM Bank" [p "Bank number for internal RAM"] [field 3 "svbk2_0" "0"]
        ]
      ]
    , td ["MBC ROM", br, value [descField "Bank Number" [] [field 4 "romBank" "0000"]]]
    , td
      [ "MBC RAM"
      , br
      , value
        [ descField "RAM Gate"
                    [ul ["O: RAM gate open.", "C: RAM gate closed."]]
                    [field 1 "ramGate" "C"]
        , descField "Bank Number" [] [field 4 "ramBank" "0000"]
        ]
      ]
    ]
  , tr
    [ td
      [ "P1"
      , br
      , value
        [ fieldGroup [unused, unused]
        , fieldGroup
          [ descField "P15" [p "Select Up, Down, Left, Right keys"] [field 1 "p15" "0"]
          , descField "P14" [p "Select A, B, Select, Start keys"]   [field 1 "p14" "0"]
          ]
        , descField "P13" [p "Down or Start pressed"] [field 1 "p13" "0"]
        , descField "P12" [p "Up or Select pressed"]  [field 1 "p12" "0"]
        , descField "P11" [p "Left or B pressed"]     [field 1 "p11" "0"]
        , descField "P10" [p "Right or A pressed"]    [field 1 "p10" "0"]
        ]
      ]
    , td ["DIV", br, value [field 8 "div" "00"]]
    , td ["TIMA", br, value [field 8 "tima" "00"]]
    , td ["TMA", br, value [field 8 "tma" "00"]]
    , td
      [ "TAC"
      , br
      , value
        (  replicate 5 unused
        <> [ descField "Timer Enable" enableDisable [field 1 "tac2" "0"]
           , descField "Timer Frequency"
                       [ul ["0: 4KiHz", "1: 256KiHz", "2: 64KiHz", "3: 16KiHz"]]
                       [field 2 "tac1_0" "0"]
           ]
        )
      ]
    ]
  , tr
    [ td ["SB", br, value ([7, 6 .. 0] <&> (\i -> field 1 ("sb" <> BB.intDec i) "0"))]
    , td
      [ "SC"
      , br
      , value
        (  descField "Transfer Enabled" enableDisable [field 1 "sc7" "0"]
        :  replicate 5 unused
        <> [ descField
             "Shift Clock"
             [ ul
                 [ "0: 8KiHz (16KiHz in double speed mode)"
                 , "1: 256KiHz (512KiHz in double speed mode)"
                 ]
             ]
             [field 1 "sc1" "0"]
           , descField "Clock Selection"
                       [ul ["0: External clock", "1: Internal clock"]]
                       [field 1 "sc0" "0"]
           ]
        )
      ]
    , td
      [ "RP"
      , br
      , value
        (  descField "Read Enable" [ul ["0: Disable", "3: Enable"]] [field 2 "rp7_6" "0"]

        :  replicate 4 unused
        <> [ descField "Read"  [ul ["0: On", "1: Off"]] [field 1 "rp1" "0"]
           , descField "Write" [ul ["0: Off", "1: On"]] [field 1 "rp0" "0"]
           ]
        )
      ]
    , td
      [ "IF"
      , br
      , value
        (  replicate 3 unused
        <> [ descField "Keypad Interrupt"    [] [field 1 "if4" "0"]
           , descField "Serial IO Interrupt" [] [field 1 "if3" "0"]
           , descField "Timer Interrupt"     [] [field 1 "if2" "0"]
           , descField "LCD STAT Interrupt"  [] [field 1 "if1" "0"]
           , descField "VBlank Interrupt"    [] [field 1 "if0" "0"]
           ]
        )
      ]
    , td
      [ "IE"
      , br
      , value
        (  replicate 3 unused
        <> [ descField "Keypad Interrupt"    enableDisable [field 1 "ie4" "0"]
           , descField "Serial IO Interrupt" enableDisable [field 1 "ie3" "0"]
           , descField "Timer Interrupt"     enableDisable [field 1 "ie2" "0"]
           , descField "LCD STAT Interrupt"  enableDisable [field 1 "ie1" "0"]
           , descField "VBlank Interrupt"    enableDisable [field 1 "ie0" "0"]
           ]
        )
      ]
    ]
  ]
