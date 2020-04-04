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
  "hardware"
  [tr [th 5 ["System Registers"]]]
  [ tr
    [ td
      [ "KEY1"
      , br
      , value
        (  field "key17"
                 (desc "0" "CPU Speed" [ul ["0: Normal speed (1MiHz)", "1: Double speed (2MiHz)"]])
        :  replicate 6 unused
        <> [ field
               "key10"
               (desc "0"
                     "Speed Switch"
                     [ul ["0: Switch to normal speed", "1: Switch to double speed"]]
               )
           ]
        )
      ]
    , td
      [ "VBK"
      , br
      , value
        (replicate 7 unused <> [field "vbk0" (desc "0" "VRAM Bank" [p "Video RAM bank number"])])
      ]
    , td
      [ "SVBK"
      , br
      , value
        [ field "svbk7" "0"
        , unused
        , field "svbk5" "0"
        , field "svbk4" "0"
        , field "svbk3" "0"
        , padding 2
        , field "svbk2_0" (desc "0" "RAM Bank" [p "Bank number for internal RAM"])
        ]
      ]
    , td ["MBC ROM", br, value [field "romBank" (desc "0000" "Bank Number" [])]]
    , td
      [ "MBC RAM"
      , br
      , value
        [ field "ramGate" (desc "0" "RAM Gate" [ul ["O: RAM gate open.", "C: RAM gate closed."]])
        , field "ramBank" (desc "0000" "Bank Number" [])
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
          [ field "p15" (desc "0" "P15" [p "Select Up, Down, Left, Right keys"])
          , field "p14" (desc "0" "P14" [p "Select A, B, Select, Start keys"])
          ]
        , field "p13" (desc "0" "P13" [p "Down or Start pressed"])
        , field "p12" (desc "0" "P12" [p "Up or Select pressed"])
        , field "p11" (desc "0" "P11" [p "Left or B pressed"])
        , field "p10" (desc "0" "P10" [p "Right or A pressed"])
        ]
      ]
    , td ["DIV", br, value [padding 6, field "div" "00"]]
    , td ["TIMA", br, value [padding 6, field "tima" "00"]]
    , td ["TMA", br, value [padding 6, field "tma" "00"]]
    , td
      [ "TAC"
      , br
      , value
        (  replicate 5 unused
        <> [ field "tac2" (desc "0" "Timer Enable" enableDisable)
           , padding 1
           , field
             "tac1_0"
             (desc "0" "Timer Frequency" [ul ["0: 4KiHz", "1: 256KiHz", "2: 64KiHz", "3: 16KiHz"]]
             )
           ]
        )
      ]
    ]
  , tr
    [ td ["SB", br, value ([7, 6 .. 0] <&> (\i -> field ("sb" <> BB.intDec i) "0"))]
    , td
      [ "SC"
      , br
      , value
        (  field "sc7" (desc "0" "Transfer Enabled" enableDisable)
        :  replicate 5 unused
        <> [ field
             "sc1"
             (desc
               "0"
               "Shift Clock"
               [ ul
                   [ "0: 8KiHz (16KiHz in double speed mode)"
                   , "1: 256KiHz (512KiHz in double speed mode)"
                   ]
               ]
             )
           , field "sc0"
                   (desc "0" "Clock Selection" [ul ["0: External clock", "1: Internal clock"]])
           ]
        )
      ]
    , td
      [ "RP"
      , br
      , value
        (  padding 1
        :  field "rp7_6" (desc "0" "Read Enable" [ul ["0: Disable", "3: Enable"]])
        :  replicate 4 unused
        <> [ field "rp1" (desc "0" "Read" [ul ["0: On", "1: Off"]])
           , field "rp0" (desc "0" "Write" [ul ["0: Off", "1: On"]])
           ]
        )
      ]
    , td
      [ "IF"
      , br
      , value
        (  replicate 3 unused
        <> [ field "if4" (desc "0" "Keypad Interrupt" [])
           , field "if3" (desc "0" "Serial IO Interrupt" [])
           , field "if2" (desc "0" "Timer Interrupt" [])
           , field "if1" (desc "0" "LCD STAT Interrupt" [])
           , field "if0" (desc "0" "VBlank Interrupt" [])
           ]
        )
      ]
    , td
      [ "IE"
      , br
      , value
        (  replicate 3 unused
        <> [ field "ie4" (desc "0" "Keypad Interrupt" enableDisable)
           , field "ie3" (desc "0" "Serial IO Interrupt" enableDisable)
           , field "ie2" (desc "0" "Timer Interrupt" enableDisable)
           , field "ie1" (desc "0" "LCD STAT Interrupt" enableDisable)
           , field "ie0" (desc "0" "VBlank Interrupt" enableDisable)
           ]
        )
      ]
    ]
  ]
