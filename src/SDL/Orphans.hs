{-# OPTIONS_GHC -fno-warn-orphans #-}
module SDL.Orphans () where

import SDL
import Data.Hashable

instance Hashable Window