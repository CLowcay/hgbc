{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.LCDRegisters
  ( lcdRegisters
  )
where

import           Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

lcdRegisters :: BB.Builder
lcdRegisters = table
  "'panel hardware'"
  [tr [th 5 ["LCD Registers"]]]
  [ tr
    [ td
      [ "LCDC"
      , br
      , value
        [ descField "LCD Enable"               enableDisable               [field 1 "lcdc7" "0"]
        , descField "Window Map"               [ul ["0: 9800", "1: 9000"]] [field 1 "lcdc6" "0"]
        , descField "Window Enable"            enableDisable               [field 1 "lcdc5" "0"]
        , descField "Background Character Set" [ul ["0: 8800", "1: 8000"]] [field 1 "lcdc4" "0"]
        , descField "Background Map"           [ul ["0: 9800", "1: 9000"]] [field 1 "lcdc3" "0"]
        , descField "Sprite Height"            [ul ["0: 8×8", "1: 8×16"]]  [field 1 "lcdc2" "0"]
        , descField "Sprites Enable"           enableDisable               [field 1 "lcdc1" "0"]
        , descField "Background Enable"        enableDisable               [field 1 "lcdc0" "0"]
        ]
      ]
    , td
      [ "HDMA5"
      , br
      , value
        [ descField "HDMA status" [ul ["0: Active", "1: Complete"]] [field 1 "hdma57" "0"]
        , descField "Bytes Transferred" [] [field 7 "hdma56_0" "00"]
        ]
      ]
    , td
      [ "HDMA3/4"
      , br
      , value
        [ descField "HDMA destination (High)" [] [field 2 "hdma3" "00"]
        , descField "HDMA destiantion (Low)"  [] [field 2 "hdma4" "00"]
        ]
      ]
    , td
      [ "HDMA1/2"
      , br
      , value
        [ descField "HDMA source (High)" [] [field 2 "hdma1" "00"]
        , descField "HDMA source (Low)"  [] [field 2 "hdma2" "00"]
        ]
      ]
    , td ["BGP", br, value (dmgPalette "bgp")]
    ]
  , tr
    [ td
      [ "STAT"
      , br
      , value
        [ unused
        , fieldGroup
          [ descField "LYC Interrupt"
                      (p "Raise interrupt 48 when LY&nbsp;=&nbsp;LYC" : enableDisable)
                      [field 1 "stat6" "0"]
          , descField
            "OAM Interrupt"
            (p "Raise interrupt 48 when LCD&nbsp;mode&nbsp;=&nbsp;2 (Scanning OAM)" : enableDisable)
            [field 1 "stat5" "0"]
          , descField
            "VBlank Interrupt"
            (p "Raise interrupt 48 when LCD&nbsp;mode&nbsp;=&nbsp;1 (VBlank)" : enableDisable)
            [field 1 "stat4" "0"]
          , descField
            "HBlank Interrupt"
            (p "Raise interrupt 48 when LCD&nbsp;mode&nbsp;=&nbsp;0 (HBlank)" : enableDisable)
            [field 1 "stat3" "0"]
          ]
        , fieldGroup
          [descField "Match Flag" [ul ["0: LYC ≠ LY", "1: LYC = LY"]] [field 1 "stat2" "0"]]
        , descField "LCD Mode"
                    [ul ["0: HBlank", "1: VBlank", "2: Scanning OAM", "3: Reading VRAM"]]
                    [field 2 "stat1_0" "0"]
        ]
      ]
    , td ["SCY", br, value [field 8 "scy" "00"]]
    , td ["WY", br, value [field 8 "wy" "00"]]
    , td ["LY", br, value [field 8 "ly" "00"]]
    , td ["OBP0", br, value (dmgPalette "obp0")]
    ]
  , tr
    [ td ["DMA", br, value [field 8 "dma" "00"]]
    , td ["SCX", br, value [field 8 "scx" "00"]]
    , td ["WX", br, value [field 8 "wx" "00"]]
    , td ["LYC", br, value [field 8 "lyc" "00"]]
    , td ["OBP1", br, value (dmgPalette "obp1")]
    ]
  , tr
    [ td ["BCPS", br, value (cps "bcps" "BCPS" "BCPD")]
    , td ["BCPD", br, value [field 8 "bcpd" "00"]]
    , td ["OCPS", br, value (cps "ocps" "OCPS" "OCPD")]
    , td ["OCPD", br, value [field 8 "ocpd" "00"]]
    , td ["&nbsp;"]
    ]
  ]
 where
  cps name thisReg pairedReg =
    [ descField
      "Auto Increment"
      [ul ["0: Do not auto-increment", "1: Increment " <> thisReg <> " on write to " <> pairedReg]]
      [field 1 (name <> "7") "0"]
    , unused
    , descField "Palette Number" [p "The palette to read/write."] [field 3 (name <> "5_3") "0"]
    , descField "Element Number"
                [p "The palette element to read/write."]
                [field 2 (name <> "2_1") "0"]
    , descField "Low/High"
                [ul ["0: Access low byte", "1: Access high byte"]]
                [field 1 (name <> "0") "0"]
    ]
  dmgPalette name = mconcat
    (   color
    <$> [ (name <> "76", "Color 3")
        , (name <> "54", "Color 2")
        , (name <> "32", "Color 1")
        , (name <> "10", "Color 0")
        ]
    )
  color (fid, name) = [descField name [] [field 2 fid "0"]]
