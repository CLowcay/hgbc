module Machine.GBC.Color
  ( Correction,
    CorrectionMode (..),
    correction,
  )
where

import Data.Word (Word16, Word32)

type Correction = (Word16, Word16, Word16) -> (Word32, Word32, Word32)

data CorrectionMode
  = NoCorrection
  | DefaultCorrection
  deriving (Eq, Ord, Show)

correction :: CorrectionMode -> Correction
correction NoCorrection =
  \(r, g, b) -> (fromIntegral (8 * r), fromIntegral (8 * g), fromIntegral (8 * b))
correction DefaultCorrection = \(r, g, b) ->
  let r' = 960 `min` (r * 26 + g * 4 + b * 2)
      g' = 960 `min` (g * 24 + b * 8)
      b' = 960 `min` (r * 6 + g * 4 + b * 22)
   in (fromIntegral (r' `div` 4), fromIntegral (g' `div` 4), fromIntegral (b' `div` 4))
