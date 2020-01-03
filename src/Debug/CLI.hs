{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Debug.CLI
  ( cli
  )
where

import           Control.Exception              ( displayException )
import           Control.Monad.Reader
import           Data.Char
import           Data.Maybe
import           Data.Void
import           Data.Word
import           Debug.Debugger
import           GBC.Errors
import           GBC.CPU.ISA
import           System.Console.Haskeline
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text                     as T
import qualified Text.Megaparsec.Char.Lexer    as L

cli :: InputT (ReaderT DebugState IO) ()
cli = do
  minput <- getInputLine "> "
  case minput of
    Nothing   -> pure ()
    Just line -> if all isSpace line
      then cli
      else do
        case parse (space *> command <* eof) "CONSOLE" (T.pack line) of
          Left err -> outputStrLn (errorBundlePretty err)
          Right cmd ->
            withInterrupt
              $         doCommand cmd
              `catches` [ Handler $ \(fault :: Fault) -> outputStrLn (displayException fault)
                        , Handler $ \(_ :: Interrupt) -> pure ()
                        ]
        cli

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (space1 <|> eof)

symbol :: T.Text -> Parser T.Text
symbol s = lexeme $ string' s

address :: Parser MemAddress
address = address0 <?> "address"
 where
  address0 = do
    maybeLabel   <- observing . try . lookAhead $ labelValue
    maybeAddress <- observing . try . lookAhead $ value16

    case (maybeLabel, maybeAddress) of
      (Right l, Right a) -> LabelAddress l (Just a) <$ labelValue
      (Right l, Left _ ) -> LabelAddress l Nothing <$ labelValue
      (Left  _, Right a) -> ConstAddress a <$ value16
      (Left  _, Left _ ) -> ConstAddress <$> value16

value :: Parser Word8
value = lexeme L.hexadecimal <?> "8-bit hexadecimal value"

value16 :: Parser Word16
value16 = lexeme L.hexadecimal <?> "16-bit hexadecimal value"

labelValue :: Parser String
labelValue = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "label"

command :: Parser Command
command =
  (ShowHeader <$ symbol "header")
    <|> (RunTo <$> try (symbol "run" *> address))
    <|> (Run <$ symbol "run")
    <|> (Reset <$ symbol "reset")
    <|> (ShowRegs <$ symbol "registers")
    <|> try (PokeR8 <$> ((symbol "reg" <|> symbol "r") *> register8) <*> (symbol "=" *> value))
    <|> try (PokeR16 <$> ((symbol "reg" <|> symbol "r") *> register16) <*> (symbol "=" *> value16))
    <|> try (ShowRegs <$ (symbol "reg" <|> symbol "r"))
    <|> try (Poke8 <$> ((symbol "byte" <|> symbol "b") *> address) <*> (symbol "=" *> value))
    <|> try (Peek8 <$> ((symbol "byte" <|> symbol "b") *> address))
    <|> try (Poke16 <$> ((symbol "word" <|> symbol "w") *> address) <*> (symbol "=" *> value16))
    <|> try (Peek16 <$> ((symbol "word" <|> symbol "w") *> address))
    <|> try (ShowMem <$> try ((symbol "memory" <|> symbol "mem" <|> symbol "m") *> address))
    <|> try (ShowDisassembly <$> (symbol "code" *> optional address))
    <|> try (ShowDisassembly <$> (symbol "c" *> optional address))
    <|> try (ListSymbols <$ symbol "symbols")
    <|> try (AddSymbol <$> try (symbol "symbol" *> labelValue) <*> try (symbol "=" *> value16))
    <|> try (StepOut <$ try (symbol "step" *> symbol "out"))
    <|> try (Step <$> (symbol "step" *> (fromMaybe 1 <$> optional L.decimal)))
    <|> try (Step <$> (symbol "s" *> (fromMaybe 1 <$> optional L.decimal)))
    <|> try (ShowGraphics <$> (symbol "graphics" *> optional labelValue))
    <|> try (ShowTimer <$> (symbol "timer" *> optional labelValue))
    <|> try (ShowInternal <$> (symbol "internal" *> optional labelValue))
    <|> try (ShowMBC <$> (symbol "mbc" *> optional labelValue))
    <|> try (AddWriteBreakpoint <$> try (symbol "watch" *> address))
    <|> try (AddBreakpoint <$> try (symbol "break" *> address))
    <|> try (DisableWriteBreakpiont <$> try (symbol "disable" *> symbol "watch" *> address))
    <|> try (DisableBreakpiont <$> try (symbol "disable" *> symbol "break" *> address))
    <|> try (ListBreakpoints <$ symbol "breakpoints")
    <|> try (DeleteWriteBreakpoint <$> try (symbol "delete" *> symbol "watch" *> address))
    <|> try (DeleteBreakpoint <$> try (symbol "delete" *> symbol "break" *> address))
    <|> try (DeleteSymbol <$> try (symbol "delete" *> symbol "symbol" *> labelValue))
    <|> try (EnableOption <$> try (symbol "enable" *> optionName))
    <|> try (DisableOption <$> try (symbol "disable" *> optionName))

optionName :: Parser Option
optionName = CheckRAMAccess <$ symbol "checkRAMAccess"

register8 :: Parser RegisterR
register8 = register <?> "8-bit register"
 where
  register =
    (RegA <$ "A")
      <|> (RegB <$ "B")
      <|> (RegC <$ "C")
      <|> (RegD <$ "D")
      <|> (RegE <$ "E")
      <|> (RegH <$ "H")
      <|> (RegL <$ "L")

register16 :: Parser RegisterSS
register16 = register <?> "16-bit register"
  where register = (RegBC <$ "BC") <|> (RegDE <$ "DE") <|> (RegHL <$ "HL") <|> (RegSP <$ "SP")
