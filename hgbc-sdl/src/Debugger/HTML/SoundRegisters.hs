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
  [ tr [td ["NR10", br], td ["NR11", br], td ["NR12", br], td ["NR13", br], td ["NR14", br]]
  , tr [td ["NR20", br], td ["NR21", br], td ["NR22", br], td ["NR23", br], td ["NR24", br]]
  , tr [td ["NR30", br], td ["NR31", br], td ["NR32", br], td ["NR33", br], td ["NR34", br]]
  , tr [td ["NR40", br], td ["NR41", br], td ["NR42", br], td ["NR43", br], td ["NR44", br]]
  , tr [td ["NR50", br], td ["NR51", br], td ["NR52", br], td ["PCM12", br], td ["PCM34", br]]
  ]
