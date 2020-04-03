{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.SoundRegisters
  ( soundRegisters
  )
where

import           Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

soundRegisters :: BB.Builder
soundRegisters = table
  "hardware"
  [tr [th 5 ["Sound Registers"]]]
  [ tr
    [ td
      [ "NR10"
      , br
      , value
        [ unused
        , padding 2
        , field "nr106_4" (desc "0" "Sweep Period" [])
        , field "nr103"   (desc "0" "Sweep Direction" [ul ["0: Up", "1: Down"]])
        , padding 2
        , field "nr102_0" (desc "0" "Sweep Shift" [])
        ]
      ]
    , td ["NR11", br, dutyLength "nr11"]
    , td ["NR12", br, envelope "nr12"]
    , td ["NR13", br, frequencyLow "nr13"]
    , td ["NR14", br, control "nr14"]
    ]
  , tr
    [ td ["NR20", br, value (replicate 8 unused)]
    , td ["NR21", br, dutyLength "nr21"]
    , td ["NR22", br, envelope "nr22"]
    , td ["NR23", br, frequencyLow "nr23"]
    , td ["NR24", br, control "nr24"]
    ]
  , tr
    [ td
      [ "NR30"
      , br
      , value (field "nr307" (desc "0" "Master Enable" enableDisable) : replicate 7 unused)
      ]
    , td ["NR31", br, value [padding 6, field "nr31" (desc "00" "Length" [])]]
    , td
      [ "NR32"
      , br
      , value (unused : padding 1 : field "nr326_5" (desc "0" "Volume" []) : replicate 5 unused)
      ]
    , td ["NR33", br, frequencyLow "nr33"]
    , td ["NR34", br, control "nr34"]
    ]
  , tr
    [ td ["NR40", br, value (replicate 8 unused)]
    , td ["NR41", br, value [unused, unused, padding 4, field "nr415_0" (desc "00" "Length" [])]]
    , td ["NR42", br, envelope "nr42"]
    , td
      [ "NR43"
      , br
      , value
        [ padding 3
        , field "nr437_4" (desc "0" "Shift Clock" [p "Base clock rate for shift register."])
        , field "nr433"
                (desc "0" "Width" [ul ["0: Feedback from bit 6", "1: Feedback from bit 14"]])
        , padding 2
        , field "nr432_0" (desc "0" "Ratio" [p "Divider ratio for shift register clock."])
        ]
      ]
    , td
      [ "NR44"
      , br
      , value
        ( field "nr447" (desc "0" "Trigger" [])
        : field "nr446" (desc "0" "Length Enable" enableDisable)
        : replicate 6 unused
        )
      ]
    ]
  , tr
    [ td
      [ "NR50"
      , br
      , value
        [ field "nr507" (desc "0" "SO2 Vin Enable" [])
        , padding 2
        , field "nr506_4" (desc "0" "SO2 Volume" [])
        , field "nr503"   (desc "0" "SO1 Vin Enable" [])
        , padding 2
        , field "nr502_0" (desc "0" "SO1 Volume" [])
        ]
      ]
    , td
      [ "NR51"
      , br
      , value
        [ fieldGroup
          [ field "nr517" (desc "0" "SO2 Channel 4 Enable" enableDisable)
          , field "nr516" (desc "0" "SO2 Channel 3 Enable" enableDisable)
          , field "nr515" (desc "0" "SO2 Channel 2 Enable" enableDisable)
          , field "nr514" (desc "0" "SO2 Channel 1 Enable" enableDisable)
          ]
        , field "nr513" (desc "0" "SO1 Channel 4 Enable" enableDisable)
        , field "nr512" (desc "0" "SO1 Channel 3 Enable" enableDisable)
        , field "nr511" (desc "0" "SO1 Channel 2 Enable" enableDisable)
        , field "nr510" (desc "0" "SO1 Channel 1 Enable" enableDisable)
        ]
      ]
    , td
      [ "NR52"
      , br
      , value
        [ field "nr527" (desc "0" "Master Power" enableDisable)
        , unused
        , unused
        , unused
        , field "nr523" (desc "0" "Channel 4 Status" isPlaying)
        , field "nr522" (desc "0" "Channel 3 Status" isPlaying)
        , field "nr521" (desc "0" "Channel 2 Status" isPlaying)
        , field "nr520" (desc "0" "Channel 1 Status" isPlaying)
        ]
      ]
    , td
      [ "PCM12"
      , br
      , value
        [ padding 3
        , field "pcm127_4" (desc "0" "Channel 2 Loopback" [])
        , padding 3
        , field "pcm123_0" (desc "0" "Channel 1 Loopback" [])
        ]
      ]
    , td
      [ "PCM34"
      , br
      , value
        [ padding 3
        , field "pcm347_4" (desc "0" "Channel 4 Loopback" [])
        , padding 3
        , field "pcm343_0" (desc "0" "Channel 3 Loopback" [])
        ]
      ]
    ]
  , tr
    [ tdspan
        5
        [ "Wave Table"
        , br
        , value
          [ field "wave0" (desc "00" "FF30" [p "Wave table samples 0 and 1."])
          , field "wave1" (desc "00" "FF31" [p "Wave table samples 2 and 3."])
          , field "wave2" (desc "00" "FF32" [p "Wave table samples 4 and 5."])
          , field "wave3" (desc "00" "FF33" [p "Wave table samples 6 and 7."])
          , field "wave4" (desc "00" "FF34" [p "Wave table samples 8 and 9."])
          , field "wave5" (desc "00" "FF35" [p "Wave table samples 10 and 11."])
          , field "wave6" (desc "00" "FF36" [p "Wave table samples 12 and 13."])
          , field "wave7" (desc "00" "FF37" [p "Wave table samples 14 and 15."])
          , field "wave8" (desc "00" "FF38" [p "Wave table samples 16 and 17."])
          , field "wave9" (desc "00" "FF39" [p "Wave table samples 18 and 19."])
          , field "waveA" (desc "00" "FF3A" [p "Wave table samples 20 and 21."])
          , field "waveB" (desc "00" "FF3B" [p "Wave table samples 22 and 23."])
          , field "waveC" (desc "00" "FF3C" [p "Wave table samples 24 and 25."])
          , field "waveD" (desc "00" "FF3D" [p "Wave table samples 26 and 27."])
          , field "waveE" (desc "00" "FF3E" [p "Wave table samples 28 and 29."])
          , field "waveF" (desc "00" "FF3F" [p "Wave table samples 30 and 31."])
          ]
        ]
    ]
  ]
 where
  isPlaying = [ul ["0: Not playing", "1: Playing"]]
  control name = value
    [ field (name <> "7") (desc "0" "Trigger" [])
    , field (name <> "6") (desc "0" "Length Enable" enableDisable)
    , unused
    , unused
    , unused
    , padding 2
    , field (name <> "2_0") (desc "0" "Frequency" [p "High 3 bits of channel frequency."])
    ]
  frequencyLow name =
    value [padding 6, field name (desc "00" "Frequency" [p "Low 8 bits of channel frequency."])]
  envelope name = value
    [ padding 3
    , field (name <> "7_4") (desc "0" "Volume" [])
    , field (name <> "3") (desc "0" "Envelope Direction" [ul ["0: Down", "1: Up"]])
    , padding 2
    , field (name <> "2_0") (desc "0" "Envelope Period" [])
    ]
  dutyLength name = value
    [ padding 1
    , field (name <> "7_6") (desc "0" "Duty Cycle" [])
    , padding 4
    , field (name <> "5_0") (desc "00" "Length" [])
    ]
