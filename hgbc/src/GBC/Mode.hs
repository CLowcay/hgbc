module GBC.Mode
  ( EmulatorMode(..)
  )
where

data EmulatorMode = DMG | CGB deriving (Eq, Ord, Show, Bounded, Enum)
