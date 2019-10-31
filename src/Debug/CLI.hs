{-# LANGUAGE OverloadedStrings #-}

module Debug.CLI
  ( nextCommand
  )
where

import           Data.Maybe
import           Data.Void
import           Data.Word
import           Debug.Debugger
import           GBC.ISA
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Text.Megaparsec.Char.Lexer    as L

nextCommand :: IO (Maybe Command)
nextCommand = do
  T.putStr "> "
  hFlush stdout
  endOfStream <- isEOF
  if endOfStream
    then pure Nothing
    else do
      line <- T.getLine
      if T.null $ T.strip line
        then nextCommand
        else case parse (space *> command) "CONSOLE" line of
          Left err -> do
            putStrLn $ errorBundlePretty err
            nextCommand
          Right cmd -> pure $ Just cmd

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (space1 <|> eof)

symbol :: T.Text -> Parser T.Text
symbol s = lexeme $ string' s

address :: Parser MemAddress
address = do
  maybeLabel   <- observing . try . lookAhead $ labelValue
  maybeAddress <- observing . try . lookAhead $ value16

  case (maybeLabel, maybeAddress) of
    (Right l, Right a) -> LabelAddress l (Just a) <$ labelValue
    (Right l, Left _ ) -> LabelAddress l Nothing <$ labelValue
    (Left  _, Right a) -> ConstAddress a <$ value16
    (Left  _, Left _ ) -> ConstAddress <$> value16

value :: Parser Word8
value = lexeme L.hexadecimal

value16 :: Parser Word16
value16 = lexeme L.hexadecimal

labelValue :: Parser String
labelValue = lexeme $ (:) <$> letterChar <*> many alphaNumChar

command :: Parser Command
command =
  (ShowHeader <$ symbol "header")
    <|> (RunTo <$> try (symbol "run" *> address))
    <|> (Run <$ symbol "run")
    <|> (Reset <$ symbol "reset")
    <|> (ShowRegs <$ symbol "registers")
    <|> try (PokeR8 <$> ((symbol "reg" <|> symbol "r") *> register8) <*> (symbol "=" *> value))
    <|> try (PokeR16 <$> ((symbol "reg" <|> symbol "r") *> register16) <*> (symbol "=" *> value16))
    <|> (ShowRegs <$ (symbol "reg" <|> symbol "r"))
    <|> try (Poke8 <$> ((symbol "byte" <|> symbol "b") *> address) <*> (symbol "=" *> value))
    <|> try (Poke16 <$> ((symbol "word" <|> symbol "w") *> address) <*> (symbol "=" *> value16))
    <|> (Peek8 <$> ((symbol "byte" <|> symbol "b") *> address))
    <|> (Peek16 <$> ((symbol "word" <|> symbol "w") *> address))
    <|> (ShowMem <$> try ((symbol "memory" <|> symbol "mem" <|> symbol "m") *> address))
    <|> (ShowDisassembly <$> (symbol "code" *> optional address))
    <|> (ShowDisassembly <$> (symbol "c" *> optional address))
    <|> (ListSymbols <$ symbol "symbols")
    <|> (AddSymbol <$> try (symbol "symbol" *> labelValue) <*> try (symbol "=" *> value16))
    <|> (StepOut <$ try (symbol "step" *> symbol "out"))
    <|> (Step <$> (symbol "step" *> (fromMaybe 1 <$> optional L.decimal)))
    <|> (Step <$> (symbol "s" *> (fromMaybe 1 <$> optional L.decimal)))
    <|> (ShowGraphics <$> (symbol "graphics" *> optional labelValue))
    <|> (ShowTimer <$> (symbol "timer" *> optional labelValue))
    <|> (ShowAudio <$> (symbol "audio" *> optional labelValue))
    <|> (ShowInternal <$> (symbol "internal" *> optional labelValue))
    <|> (ShowMBC <$> (symbol "mbc" *> optional labelValue))
    <|> (AddWriteBreakpoint <$> try (symbol "break" *> symbol "write" *> address))
    <|> (AddBreakpoint <$> try (symbol "break" *> address))
    <|> (   DisableWriteBreakpiont
        <$> try (symbol "disable" *> symbol "write" *> symbol "break" *> address)
        )
    <|> (DisableBreakpiont <$> try (symbol "disable" *> symbol "break" *> address))
    <|> (ListBreakpoints <$ symbol "breakpoints")
    <|> (   DeleteWriteBreakpoint
        <$> try (symbol "delete" *> symbol "write" *> symbol "break" *> address)
        )
    <|> (DeleteBreakpoint <$> try (symbol "delete" *> symbol "break" *> address))
    <|> (DeleteSymbol <$> try (symbol "delete" *> symbol "symbol" *> labelValue))

register8 :: Parser Register8
register8 =
  (RegA <$ "A")
    <|> (RegB <$ "B")
    <|> (RegC <$ "C")
    <|> (RegD <$ "D")
    <|> (RegE <$ "E")
    <|> (RegH <$ "H")
    <|> (RegL <$ "L")

register16 :: Parser Register16
register16 = (RegBC <$ "BC") <|> (RegDE <$ "DE") <|> (RegHL <$ "HL") <|> (RegSP <$ "SP")
