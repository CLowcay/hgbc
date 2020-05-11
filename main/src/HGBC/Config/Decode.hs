{-# LANGUAGE OverloadedStrings #-}

module HGBC.Config.Decode
  ( decodeMode
  , decodeColorCorrection
  , decodeColor
  )
where

import           Control.Monad
import           Data.Bits
import           Data.Word
import           Machine.GBC.Mode
import           Machine.GBC.Util
import qualified Data.Text                     as T
import qualified Data.Text.Read                as T
import qualified Machine.GBC.Color             as Color

decodeMode :: T.Text -> Either String (Maybe EmulatorMode)
decodeMode "auto" = Right Nothing
decodeMode "dmg"  = Right (Just DMG)
decodeMode "cgb"  = Right (Just CGB)
decodeMode x      = Left ("Unknown graphics mode " <> show x)

decodeColorCorrection :: T.Text -> Either String Color.CorrectionMode
decodeColorCorrection "none"    = Right Color.NoCorrection
decodeColorCorrection "default" = Right Color.DefaultCorrection
decodeColorCorrection x         = Left ("Unknown color correction mode " <> show x)

decodeColor :: T.Text -> Either String Word32
decodeColor t = case T.stripPrefix "#" t of
  Nothing   -> Left "Colors must start with #"
  Just code -> case T.hexadecimal code of
    Left  err    -> Left err
    Right (n, r) -> do
      when (r /= "") $ Left "Colors must start with # and contain only valid hexadecimal digits."
      case T.length code of
        3 -> Right (expand3 n)
        6 -> Right (expand6 n)
        8 -> Right n
        x -> Left ("Colors must have 3, 6, or 8 hexadecimal digits, not " <> show x <> " digits.")
 where
  expand3 x =
    let r = x .>>. 8
        g = (x .>>. 4) .&. 0xF
        b = x .&. 0xF
    in  (r .<<. 28)
          .|. (r .<<. 24)
          .|. (g .<<. 20)
          .|. (g .<<. 16)
          .|. (b .<<. 12)
          .|. (b .<<. 8)
          .|. 0xFF
  expand6 x =
    let r = x .>>. 16
        g = (x .>>. 8) .&. 0xFF
        b = x .&. 0xFF
    in  (r .<<. 24) .|. (g .<<. 16) .|. (b .<<. 8) .|. 0xFF
