{-# LANGUAGE PatternSynonyms #-}

-- | This module contains pattern synonyms for the hardware registers.
module GBC.Registers (
    pattern P1
  , pattern SB
  , pattern SC
  , pattern DIV
  , pattern TIMA
  , pattern TMA
  , pattern TAC
  , pattern NR10
  , pattern NR11
  , pattern NR12
  , pattern NR13
  , pattern NR14
  , pattern NR20
  , pattern NR21
  , pattern NR22
  , pattern NR23
  , pattern NR24
  , pattern NR30
  , pattern NR31
  , pattern NR32
  , pattern NR33
  , pattern NR34
  , pattern NR40
  , pattern NR41
  , pattern NR42
  , pattern NR43
  , pattern NR44
  , pattern NR50
  , pattern NR51
  , pattern NR52
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
  , pattern KEY1
  , pattern VBK
  , pattern HDMA1
  , pattern HDMA2
  , pattern HDMA3
  , pattern HDMA4
  , pattern HDMA5
  , pattern RP
  , pattern BCPS
  , pattern BCPD
  , pattern OCPS
  , pattern OCPD
  , pattern SVBK
  , pattern IE
  ) where

import Data.Word

pattern P1 :: Word16
pattern P1 = 0xFF00

pattern SB :: Word16
pattern SB = 0xFF01

pattern SC :: Word16
pattern SC = 0xFF02

pattern DIV :: Word16
pattern DIV = 0xFF04

pattern TIMA :: Word16
pattern TIMA = 0xFF05

pattern TMA :: Word16
pattern TMA = 0xFF06

pattern TAC :: Word16
pattern TAC = 0xFF07

pattern NR10 :: Word16
pattern NR10 = 0xFF10

pattern NR11 :: Word16
pattern NR11 = 0xFF11

pattern NR12 :: Word16
pattern NR12 = 0xFF12

pattern NR13 :: Word16
pattern NR13 = 0xFF13

pattern NR14 :: Word16
pattern NR14 = 0xFF14

pattern NR20 :: Word16
pattern NR20 = 0xFF15

pattern NR21 :: Word16
pattern NR21 = 0xFF16

pattern NR22 :: Word16
pattern NR22 = 0xFF17

pattern NR23 :: Word16
pattern NR23 = 0xFF18

pattern NR24 :: Word16
pattern NR24 = 0xFF19

pattern NR30 :: Word16
pattern NR30 = 0xFF1A

pattern NR31 :: Word16
pattern NR31 = 0xFF1B

pattern NR32 :: Word16
pattern NR32 = 0xFF1C

pattern NR33 :: Word16
pattern NR33 = 0xFF1D

pattern NR34 :: Word16
pattern NR34 = 0xFF1E

pattern NR40 :: Word16
pattern NR40 = 0xFF1F

pattern NR41 :: Word16
pattern NR41 = 0xFF20

pattern NR42 :: Word16
pattern NR42 = 0xFF21

pattern NR43 :: Word16
pattern NR43 = 0xFF22

pattern NR44 :: Word16
pattern NR44 = 0xFF23

pattern NR50 :: Word16
pattern NR50 = 0xFF24

pattern NR51 :: Word16
pattern NR51 = 0xFF25

pattern NR52 :: Word16
pattern NR52 = 0xFF26

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

pattern KEY1 :: Word16
pattern KEY1 = 0xFF4D

pattern VBK :: Word16
pattern VBK = 0xFF4F

pattern HDMA1 :: Word16
pattern HDMA1 = 0xFF51

pattern HDMA2 :: Word16
pattern HDMA2 = 0xFF52

pattern HDMA3 :: Word16
pattern HDMA3 = 0xFF53

pattern HDMA4 :: Word16
pattern HDMA4 = 0xFF54

pattern HDMA5 :: Word16
pattern HDMA5 = 0xFF55

pattern RP :: Word16
pattern RP = 0xFF56

pattern BCPS :: Word16
pattern BCPS = 0xFF68

pattern BCPD :: Word16
pattern BCPD = 0xFF69

pattern OCPS :: Word16
pattern OCPS = 0xFF6A

pattern OCPD :: Word16
pattern OCPD = 0xFF5B

pattern SVBK :: Word16
pattern SVBK = 0xFF70

pattern IE :: Word16
pattern IE = 0xFFFF