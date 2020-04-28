{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.HTML.SoundRegisters
  ( soundRegisters
  )
where

import           HGBC.Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

soundRegisters :: BB.Builder
soundRegisters = table
  "'panel hardware'"
  [tr [th 5 ["Sound Registers"]]]
  [ tr
    [ td
      [ "NR10"
      , br
      , value
        [ unused
        , descField "Sweep Period"    []                        [field 3 "nr106_4" "0"]
        , descField "Sweep Direction" [ul ["0: Up", "1: Down"]] [field 1 "nr103" "0"]
        , descField "Sweep Shift"     []                        [field 3 "nr102_0" "0"]
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
      , value (descField "Master Enable" enableDisable [field 1 "nr307" "0"] : replicate 7 unused)
      ]
    , td ["NR31", br, value [descField "Length" [] [field 8 "nr31" "00"]]]
    , td
      [ "NR32"
      , br
      , value (unused : descField "Volume" [] [field 2 "nr326_5" "0"] : replicate 5 unused)
      ]
    , td ["NR33", br, frequencyLow "nr33"]
    , td ["NR34", br, control "nr34"]
    ]
  , tr
    [ td ["NR40", br, value (replicate 8 unused)]
    , td ["NR41", br, value [unused, unused, descField "Length" [] [field 6 "nr415_0" "00"]]]
    , td ["NR42", br, envelope "nr42"]
    , td
      [ "NR43"
      , br
      , value
        [ descField "Shift Clock" [p "Base clock rate for shift register."] [field 4 "nr437_4" "0"]
        , descField "Width"
                    [ul ["0: Feedback from bit 6", "1: Feedback from bit 14"]]
                    [field 1 "nr433" "0"]
        , descField "Ratio" [p "Divider ratio for shift register clock."] [field 3 "nr432_0" "0"]
        ]
      ]
    , td
      [ "NR44"
      , br
      , value
        ( descField "Trigger"       []            [field 1 "nr447" "0"]
        : descField "Length Enable" enableDisable [field 1 "nr446" "0"]
        : replicate 6 unused
        )
      ]
    ]
  , tr
    [ td
      [ "NR50"
      , br
      , value
        [ descField "SO2 Vin Enable" [] [field 1 "nr507" "0"]
        , descField "SO2 Volume"     [] [field 3 "nr506_4" "0"]
        , descField "SO1 Vin Enable" [] [field 1 "nr503" "0"]
        , descField "SO1 Volume"     [] [field 3 "nr502_0" "0"]
        ]
      ]
    , td
      [ "NR51"
      , br
      , value
        [ fieldGroup
          [ descField "SO2 Channel 4 Enable" enableDisable [field 1 "nr517" "0"]
          , descField "SO2 Channel 3 Enable" enableDisable [field 1 "nr516" "0"]
          , descField "SO2 Channel 2 Enable" enableDisable [field 1 "nr515" "0"]
          , descField "SO2 Channel 1 Enable" enableDisable [field 1 "nr514" "0"]
          ]
        , descField "SO1 Channel 4 Enable" enableDisable [field 1 "nr513" "0"]
        , descField "SO1 Channel 3 Enable" enableDisable [field 1 "nr512" "0"]
        , descField "SO1 Channel 2 Enable" enableDisable [field 1 "nr511" "0"]
        , descField "SO1 Channel 1 Enable" enableDisable [field 1 "nr510" "0"]
        ]
      ]
    , td
      [ "NR52"
      , br
      , value
        [ descField "Master Power" enableDisable [field 1 "nr527" "0"]
        , unused
        , unused
        , unused
        , descField "Channel 4 Status" isPlaying [field 1 "nr523" "0"]
        , descField "Channel 3 Status" isPlaying [field 1 "nr522" "0"]
        , descField "Channel 2 Status" isPlaying [field 1 "nr521" "0"]
        , descField "Channel 1 Status" isPlaying [field 1 "nr520" "0"]
        ]
      ]
    , td
      [ "PCM12"
      , br
      , value
        [ descField "Channel 2 Loopback" [] [field 4 "pcm127_4" "0"]
        , descField "Channel 1 Loopback" [] [field 4 "pcm123_0" "0"]
        ]
      ]
    , td
      [ "PCM34"
      , br
      , value
        [ descField "Channel 4 Loopback" [] [field 4 "pcm347_4" "0"]
        , descField "Channel 3 Loopback" [] [field 4 "pcm343_0" "0"]
        ]
      ]
    ]
  , tr
    [ tdspan
        5
        [ "Wave Table"
        , br
        , value
          [ descField "FF30" [p "Wave table samples 0 and 1."]   [field 2 "wave0" "00"]
          , descField "FF31" [p "Wave table samples 2 and 3."]   [field 2 "wave1" "00"]
          , descField "FF32" [p "Wave table samples 4 and 5."]   [field 2 "wave2" "00"]
          , descField "FF33" [p "Wave table samples 6 and 7."]   [field 2 "wave3" "00"]
          , descField "FF34" [p "Wave table samples 8 and 9."]   [field 2 "wave4" "00"]
          , descField "FF35" [p "Wave table samples 10 and 11."] [field 2 "wave5" "00"]
          , descField "FF36" [p "Wave table samples 12 and 13."] [field 2 "wave6" "00"]
          , descField "FF37" [p "Wave table samples 14 and 15."] [field 2 "wave7" "00"]
          , descField "FF38" [p "Wave table samples 16 and 17."] [field 2 "wave8" "00"]
          , descField "FF39" [p "Wave table samples 18 and 19."] [field 2 "wave9" "00"]
          , descField "FF3A" [p "Wave table samples 20 and 21."] [field 2 "waveA" "00"]
          , descField "FF3B" [p "Wave table samples 22 and 23."] [field 2 "waveB" "00"]
          , descField "FF3C" [p "Wave table samples 24 and 25."] [field 2 "waveC" "00"]
          , descField "FF3D" [p "Wave table samples 26 and 27."] [field 2 "waveD" "00"]
          , descField "FF3E" [p "Wave table samples 28 and 29."] [field 2 "waveE" "00"]
          , descField "FF3F" [p "Wave table samples 30 and 31."] [field 2 "waveF" "00"]
          ]
        ]
    ]
  ]
 where
  isPlaying = [ul ["0: Not playing", "1: Playing"]]
  control name = value
    [ descField "Trigger"       []            [field 1 (name <> "7") "0"]
    , descField "Length Enable" enableDisable [field 1 (name <> "6") "0"]
    , unused
    , unused
    , unused
    , descField "Frequency" [p "High 3 bits of channel frequency."] [field 3 (name <> "2_0") "0"]
    ]
  frequencyLow name =
    value [descField "Frequency" [p "Low 8 bits of channel frequency."] [field 8 name "00"]]
  envelope name = value
    [ descField "Volume"             []                        [field 4 (name <> "7_4") "0"]
    , descField "Envelope Direction" [ul ["0: Down", "1: Up"]] [field 1 (name <> "3") "0"]
    , descField "Envelope Period"    []                        [field 3 (name <> "2_0") "0"]
    ]
  dutyLength name = value
    [ descField "Duty Cycle" [] [field 2 (name <> "7_6") "0"]
    , descField "Length"     [] [field 6 (name <> "5_0") "00"]
    ]
