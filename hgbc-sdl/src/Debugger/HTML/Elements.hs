{-# LANGUAGE OverloadedStrings #-}

module Debugger.HTML.Elements
  ( html
  , head
  , title
  , charset
  , meta
  , link
  , script
  , body
  , h
  , nav
  , iframe
  , br
  , p
  , table
  , tr
  , td
  , tdspan
  , th
  , ul
  , ulid
  , div
  , divclass
  , divclassid
  , spanclass
  , img
  , label
  , input
  , value
  , field
  , fieldGroup
  , desc
  , enableDisable
  , unused
  , padding
  , form
  , button
  )
where

import           Data.List               hiding ( head )
import           Prelude                 hiding ( head
                                                , div
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB

html :: [BB.Builder] -> BB.Builder
html contents = "<!DOCTYPE html><html>" <> mconcat contents <> "</html>"

head :: [BB.Builder] -> BB.Builder
head heads = "<head>" <> mconcat heads <> "</head>"

title :: BB.Builder -> BB.Builder
title t = "<title>" <> t <> "</title>"

charset :: BB.Builder -> BB.Builder
charset c = "<meta charset='" <> c <> "'>"

meta :: BB.Builder -> BB.Builder -> BB.Builder
meta name content = "<meta name='" <> name <> "' content='" <> content <> "'>"

link :: BB.Builder -> BB.Builder -> BB.Builder
link rel href = "<link rel='" <> rel <> "' href='" <> href <> "'>"

script :: BB.Builder -> BB.Builder
script src = "<script src='" <> src <> "'></script>"

body :: [BB.Builder] -> BB.Builder
body contents = "<body>" <> mconcat contents <> "</body>"

h :: Int -> BB.Builder -> BB.Builder
h n t = "<h" <> BB.intDec n <> ">" <> t <> "</h" <> BB.intDec n <> ">"

nav :: [BB.Builder] -> BB.Builder
nav contents = "<nav>" <> mconcat contents <> "</nav>"

iframe :: BB.Builder -> BB.Builder
iframe name = "<iframe name='" <> name <> "'></iframe>"

br :: BB.Builder
br = "<br>"

p :: BB.Builder -> BB.Builder
p t = "<p>" <> t

table :: BB.Builder -> [BB.Builder] -> [BB.Builder] -> BB.Builder
table tid heads rows =
  "<table class="
    <> tid
    <> "><thead>"
    <> mconcat heads
    <> "</thead><tbody>"
    <> mconcat rows
    <> "</tbody></table>"

tr :: [BB.Builder] -> BB.Builder
tr cells = "<tr>" <> mconcat cells <> "</tr>"

td :: [BB.Builder] -> BB.Builder
td contents = "<td>" <> mconcat contents <> "</td>"

tdspan :: Int -> [BB.Builder] -> BB.Builder
tdspan colspan contents = "<td colspan=" <> BB.intDec colspan <> ">" <> mconcat contents <> "</td>"

th :: Int -> [BB.Builder] -> BB.Builder
th colspan contents = "<th colspan=" <> BB.intDec colspan <> ">" <> mconcat contents <> "</th>"

ul :: [BB.Builder] -> BB.Builder
ul items = "<ul>" <> mconcat (("<li>" <>) <$> items) <> "</ul>"

ulid :: BB.Builder -> [BB.Builder] -> BB.Builder
ulid uid items = "<ul id=" <> uid <> ">" <> mconcat (("<li>" <>) <$> items) <> "</ul>"

div :: [BB.Builder] -> BB.Builder
div contents = "<div>" <> mconcat contents <> "</div>"

divclass :: BB.Builder -> [BB.Builder] -> BB.Builder
divclass c contents = "<div class=" <> c <> ">" <> mconcat contents <> "</div>"

divclassid :: BB.Builder -> [BB.Builder] -> [BB.Builder] -> BB.Builder
divclassid did c contents = "<div id=" <> did <> classes c <> ">" <> mconcat contents <> "</div>"
 where
  classes []   = ""
  classes [cl] = " class=" <> cl
  classes cls  = " class='" <> mconcat (intersperse " " cls) <> "'"

spanclass :: BB.Builder -> [BB.Builder] -> BB.Builder
spanclass c contents = "<span class=" <> c <> ">" <> mconcat contents <> "</span>"

img :: BB.Builder -> BB.Builder
img url = "<img src='" <> url <> "'>"

label :: [BB.Builder] -> BB.Builder
label contents = "<label>" <> mconcat contents <> "</label>"

input :: BB.Builder -> BB.Builder -> Int -> BB.Builder -> BB.Builder
input inputType name size v =
  "<input id="
    <> name
    <> " name= "
    <> name
    <> " type="
    <> inputType
    <> " size="
    <> BB.intDec size
    <> " value='"
    <> v
    <> "'>"

value :: [BB.Builder] -> BB.Builder
value = divclass "value"

fieldGroup :: [BB.Builder] -> BB.Builder
fieldGroup contents = "<span class=group>" <> mconcat contents <> "</span>"

field :: BB.Builder -> BB.Builder -> BB.Builder
field fid iv = "<span id=" <> fid <> ">" <> iv <> "</span>"

desc :: BB.Builder -> BB.Builder -> [BB.Builder] -> BB.Builder
desc iv name description = iv <> divclass "info" (h 3 name : description)

enableDisable :: [BB.Builder]
enableDisable = [ul ["0: Disable", "1: Enable"]]

unused :: BB.Builder
unused = "<span class=u>&nbsp;</span>"

padding :: Int -> BB.Builder
padding n = "<span class=padding>" <> mconcat (replicate n "&nbsp;") <> "</span>"

form :: B.ByteString -> BB.Builder -> BB.Builder -> [BB.Builder] -> BB.Builder
form method action target contents =
  "<form method="
    <> BB.byteString method
    <> " action='"
    <> action
    <> "' target='"
    <> target
    <> "'>"
    <> mconcat contents
    <> "</form>"

button :: BB.Builder -> BB.Builder -> [BB.Builder] -> BB.Builder
button name tt content =
  "<button name='" <> name <> "' title='" <> tt <> "'>" <> mconcat content <> "</button>"
