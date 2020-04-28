{-# LANGUAGE OverloadedStrings #-}

module HGBC.Debugger.JSON
  ( toLazyByteString
  , longAddress
  , labels
  , breakpoints
  , fields
  )
where

import           Data.Aeson
import           Machine.GBC.Disassembler
import           Machine.GBC.Util               ( formatHex )
import qualified Data.Aeson.Encoding           as JSON
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Short         as SB
import qualified Data.Text                     as T

toLazyByteString :: Encoding -> LB.ByteString
toLazyByteString = BB.toLazyByteString . JSON.fromEncoding

longAddress :: LongAddress -> Encoding
longAddress (LongAddress bank offset) = JSON.pairs ("bank" .= bank <> "offset" .= offset)

labels :: [(LongAddress, (T.Text, Editable))] -> Encoding
labels = JSON.list
  (\(address, (text, isEditable)) -> pairs
    (JSON.pair "address" (longAddress address) <> "text" .= text <> "isEditable" .= isEditable)
  )

breakpoints :: [(LongAddress, Bool)] -> Encoding
breakpoints = JSON.list
  (\(address, isEnabled) ->
    pairs (JSON.pair "address" (longAddress address) <> "isEnabled" .= isEnabled)
  )

parameter :: Parameter -> Encoding
parameter (Constant text) = pairs ("text" .= text)
parameter (Address address@(LongAddress _ offset)) =
  pairs ("text" .= ('$' : formatHex offset) <> JSON.pair "address" (longAddress address))
parameter (AtAddress address@(LongAddress _ offset)) = pairs
  (  ("text" .= ("($" ++ formatHex offset ++ ")"))
  <> JSON.pair "address" (longAddress address)
  <> ("indirect" .= True)
  )

fields :: [Field] -> Encoding
fields = JSON.list field

field :: Field -> Encoding
field (Field address bytes overlap fdata) = pairs
  (  JSON.pair "address" (longAddress address)
  <> ("bytes" .= unwords (formatHex <$> SB.unpack bytes))
  <> ("overlap" .= overlap)
  <> fdataencoding
  )
 where
  fdataencoding = case fdata of
    Data                    -> "text" .= ("db" :: T.Text)
    Instruction0 text       -> "text" .= text <> JSON.pair "p" (JSON.list id [] :: Encoding)
    Instruction1 text p1    -> "text" .= text <> JSON.pair "p" (JSON.list parameter [p1])
    Instruction2 text p1 p2 -> "text" .= text <> JSON.pair "p" (JSON.list parameter [p1, p2])
