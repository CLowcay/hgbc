{-# LANGUAGE PatternSynonyms #-}

-- | This module contains pattern synonyms for the hardware registers.
module GBC.Registers (
    pattern P1
  , pattern DIV
  , pattern TIMA
  , pattern TMA
  , pattern TAC
  , pattern IF
  , pattern LCDC
  , pattern STAT
  , pattern SCY
  , pattern SCX
  , pattern LY
  , pattern LYC
  , pattern DMA
  , pattern BGP
  , pattern OBP0
  , pattern OBP1
  , pattern WY
  , pattern WX
  , pattern IE
  ) where

import Data.Word

pattern P1 :: Word16
pattern P1 = 0xFF00

pattern DIV :: Word16
pattern DIV = 0xFF04

pattern TIMA :: Word16
pattern TIMA = 0xFF05

pattern TMA :: Word16
pattern TMA = 0xFF06

pattern TAC :: Word16
pattern TAC = 0xFF07

pattern IF :: Word16
pattern IF = 0xFF0F

pattern LCDC :: Word16
pattern LCDC = 0xFF40

pattern STAT :: Word16
pattern STAT = 0xFF41

pattern SCY :: Word16
pattern SCY = 0xFF42

pattern SCX :: Word16
pattern SCX = 0xFF43

pattern LY :: Word16
pattern LY = 0xFF44

pattern LYC :: Word16
pattern LYC = 0xFF45

pattern DMA :: Word16
pattern DMA = 0xFF46

pattern BGP :: Word16
pattern BGP = 0xFF47

pattern OBP0 :: Word16
pattern OBP0 = 0xFF48

pattern OBP1 :: Word16
pattern OBP1 = 0xFF49

pattern WY :: Word16
pattern WY = 0xFF4A

pattern WX :: Word16
pattern WX = 0xFF4B

pattern IE :: Word16
pattern IE = 0xFFFF