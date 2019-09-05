{-# LANGUAGE OverloadedStrings #-}

module Debug.CLI
  ( nextCommand
  )
where

import           Data.Void
import           Data.Word
import           Debug.Commands
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           System.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Maybe
import qualified Text.Megaparsec.Char.Lexer    as L
import           GBC.ISA

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

address :: Parser Word16
address = lexeme L.hexadecimal

value :: Parser Word8
value = lexeme L.hexadecimal

value16 :: Parser Word16
value16 = lexeme L.hexadecimal

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
    <|> try
          (   Poke8
          <$> ((symbol "memory" <|> symbol "mem" <|> symbol "m") *> address)
          <*> (symbol "=" *> value)
          )
    <|> (ShowMem <$> ((symbol "memory" <|> symbol "mem" <|> symbol "m") *> address))
    <|> (ShowDisassembly <$> (symbol "code" *> optional address))
    <|> (ShowDisassembly <$> (symbol "c" *> optional address))
    <|> (Step <$> (symbol "step" *> (fromMaybe 1 <$> optional L.decimal)))
    <|> (Step <$> (symbol "s" *> (fromMaybe 1 <$> optional L.decimal)))
    <|> (AddBreakpoint <$> try (symbol "break" *> address))
    <|> (DeleteBreakpoint <$> try (symbol "delete" *> symbol "break" *> address))
    <|> (DisableBreakpiont <$> try (symbol "disable" *> symbol "break" *> address))
    <|> (ListBreakpoints <$ symbol "breakpoints")

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
