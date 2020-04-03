{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.LCDRegisters
  ( lcdRegisters
  )
where

import           Debugger.HTML.Elements
import qualified Data.ByteString.Builder       as BB

lcdRegisters :: BB.Builder
lcdRegisters = table
  "hardware"
  [tr [th 5 ["LCD Registers"]]]
  [ tr
    [ td
      [ "LCDC"
      , br
      , value
        [ field "lcdc7" (desc "0" "LCD Enable" enableDisable)
        , field "lcdc6" (desc "0" "Window Map" [ul ["0: 9800", "1: 9000"]])
        , field "lcdc5" (desc "0" "Window Enable" enableDisable)
        , field "lcdc4" (desc "0" "Background Character Set" [ul ["0: 8800", "1: 8000"]])
        , field "lcdc3" (desc "0" "Background Map" [ul ["0: 9800", "1: 9000"]])
        , field "lcdc2" (desc "0" "Sprite Height" [ul ["0: 8×8", "1: 8×16"]])
        , field "lcdc1" (desc "0" "Sprites Enable" enableDisable)
        , field "lcdc0" (desc "0" "Background Enable" enableDisable)
        ]
      ]
    , td
      [ "HDMA5"
      , br
      , value
        [ field "hdma57" (desc "0" "HDMA status" [ul ["0: Active", "1: Complete"]])
        , padding 5
        , field "hdma56_0" (desc "00" "Bytes Transferred" [])
        ]
      ]
    , td
      [ "HDMA3/4"
      , br
      , value
        [ field "hdma3" (desc "00" "HDMA destination (High)" [])
        , field "hdma4" (desc "00" "HDMA destiantion (Low)" [])
        ]
      ]
    , td
      [ "HDMA1/2"
      , br
      , value
        [ field "hdma1" (desc "00" "HDMA source (High)" [])
        , field "hdma2" (desc "00" "HDMA source (Low)" [])
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
          [ field
            "stat6"
            (desc "0"
                  "LYC Interrupt"
                  (p "Raise interrupt 48 when LY&nbsp;=&nbsp;LYC" : enableDisable)
            )
          , field
            "stat5"
            (desc
              "0"
              "OAM Interrupt"
              ( p "Raise interrupt 48 when LCD&nbsp;mode&nbsp;=&nbsp;2 (Scanning OAM)"
              : enableDisable
              )
            )
          , field
            "stat4"
            (desc
              "0"
              "VBlank Interrupt"
              (p "Raise interrupt 48 when LCD&nbsp;mode&nbsp;=&nbsp;1 (VBlank)" : enableDisable)
            )
          , field
            "stat3"
            (desc
              "0"
              "HBlank Interrupt"
              (p "Raise interrupt 48 when LCD&nbsp;mode&nbsp;=&nbsp;0 (HBlank)" : enableDisable)
            )
          ]
        , fieldGroup [field "stat2" (desc "0" "Match Flag" [ul ["0: LYC ≠ LY", "1: LYC = LY"]])]
        , padding 1
        , field
          "stat1_0"
          (desc "0" "LCD Mode" [ul ["0: HBlank", "1: VBlank", "2: Scanning OAM", "3: Reading VRAM"]]
          )
        ]
      ]
    , td ["SCY", br, value [padding 6, field "scy" "00"]]
    , td ["WY", br, value [padding 6, field "wy" "00"]]
    , td ["LY", br, value [padding 6, field "ly" "00"]]
    , td ["OBP0", br, value (dmgPalette "obp0")]
    ]
  , tr
    [ td ["DMA", br, value [padding 6, field "dma" "00"]]
    , td ["SCX", br, value [padding 6, field "scx" "00"]]
    , td ["WX", br, value [padding 6, field "wx" "00"]]
    , td ["LYC", br, value [padding 6, field "lyc" "00"]]
    , td ["OBP1", br, value (dmgPalette "obp1")]
    ]
  , tr
    [ td ["BCPS", br, value (cps "bcps" "BCPS" "BCPD")]
    , td ["BCPD", br, value [padding 6, field "bcpd" "00"]]
    , td ["OCPS", br, value (cps "ocps" "OCPS" "OCPD")]
    , td ["OCPD", br, value [padding 6, field "ocpd" "00"]]
    , td ["&nbsp;"]
    ]
  ]
 where
  cps name thisReg pairedReg =
    [ field
      (name <> "7")
      (desc
        "0"
        "Auto Increment"
        [ ul
            ["0: Do not auto-increment", "1: Increment " <> thisReg <> " on write to " <> pairedReg]
        ]
      )
    , unused
    , padding 2
    , field (name <> "5_3") (desc "0" "Palette Number" [p "The palette to read/write."])
    , padding 1
    , field (name <> "2_1") (desc "0" "Element Number" [p "The palette element to read/write."])
    , field (name <> "0")   (desc "0" "Low/High" [ul ["0: Access low byte", "1: Access high byte"]])
    ]
  dmgPalette name = mconcat
    (   color
    <$> [ (name <> "76", "Color 3")
        , (name <> "54", "Color 2")
        , (name <> "32", "Color 1")
        , (name <> "10", "Color 0")
        ]
    )
  color (label, name) = [padding 1, field label (desc "0" name [])]
