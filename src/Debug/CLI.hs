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
import qualified Text.Megaparsec.Char.Lexer    as L

nextCommand :: IO (Maybe Command)
nextCommand = do
  endOfStream <- isEOF
  if endOfStream
    then pure Nothing
    else do
      line <- T.hGetLine stdin
      let result = parse (space *> command) "CONSOLE" line
      case result of
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
address = L.hexadecimal

command :: Parser Command
command =
  (ShowHeader <$ symbol "header")
    <|> (ShowRegs <$ symbol "registers")
    <|> (ShowRegs <$ symbol "r")
    <|> (ShowMem <$> (symbol "memory" *> address))
    <|> (ShowMem <$> (symbol "m" *> address))
    <|> (ShowDisassembly . Just <$> (symbol "code" *> address))
    <|> (ShowDisassembly Nothing <$ symbol "code")
    <|> (ShowDisassembly . Just <$> try (symbol "c" *> address))
    <|> (ShowDisassembly Nothing <$ symbol "c")
    <|> (Step <$> try (symbol "step" *> L.decimal))
    <|> (Step <$> try (symbol "s" *> L.decimal))
    <|> (Step 1 <$ symbol "step")
    <|> (Step 1 <$ symbol "s")
    <|> (Reset <$ symbol "reset")
