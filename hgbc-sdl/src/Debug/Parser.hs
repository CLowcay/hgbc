{-# LANGUAGE OverloadedStrings #-}

module Debug.Parser
  ( statement
  , statements
  , expression
  )
where

import           Control.Monad.Combinators.Expr
import           Data.Void
import           Data.Word
import           Debug.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text                     as T
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 (L.skipLineComment "#") empty)

number :: Parser Int
number = lexeme ((lexeme "$" *> L.hexadecimal) <|> L.decimal) <?> "number"

symbol :: Parser T.Text
symbol = label "symbol"
  $ lexeme (T.pack <$> ((:) <$> choice [letterChar, char '_', char '.'] <*> many alphaNumChar))

stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> lexeme (char '"' *> L.charLiteral `manyTill` char '"') <?> "string"

word :: Parser Word16
word = label "word" $ do
  n <- number
  if n > 0xFFFF then fail "value out of range. Maximum value is 0xFFFF" else pure (fromIntegral n)

address :: Parser Address
address = (longAddress <|> (ShortAddress <$> word)) <?> "address"

longAddress :: Parser Address
longAddress = try (LongAddress <$> word <*> (lexeme ":" *> word)) <?> "long address"

value :: Parser Value
value = label "value" $ choice
  [ BoolValue True <$ lexeme "true"
  , BoolValue False <$ lexeme "false"
  , AddressValue <$> longAddress
  , do
    n <- number
    pure $ if n <= 0xFF
      then ByteValue (fromIntegral n)
      else if n <= 0xFFFF then WordValue (fromIntegral n) else IntValue n
  ]

register8 :: Parser ExpandedRegister8
register8 = label "register" $ lexeme
  (choice
    [ReA <$ "A", ReB <$ "B", ReC <$ "C", ReD <$ "D", ReE <$ "E", ReF <$ "F", ReH <$ "H", ReL <$ "L"]
  )

register16 :: Parser ExpandedRegister16
register16 = label "wide register" $ lexeme
  (choice [ReAF <$ "AF", ReBC <$ "BC", ReDE <$ "DE", ReHL <$ "HL", ReSP <$ "SP", RePC <$ "PC"])

expression :: Parser Expression
expression = makeExprParser term opTable
 where
  term = choice
    [ Register8Expr <$> register8
    , Register16Expr <$> register16
    , ValueExpr <$> value
    , SymbolExpr <$> symbol
    , between (lexeme "[") (lexeme "]") (DereferenceExpr <$> expression)
    , between (lexeme "(") (lexeme ")") (ParenthesizedExpr <$> expression)
    ]
  opTable =
    [ [unop "~" OpNot]
    , [binOpL "<<" OpShiftLeft, binOpL ">>" OpShiftRight]
    , [binOpL "*" OpMultiply, binOpL "/" OpDivide]
    , [binOpL "+" OpPlus, binOpL "-" OpMinus]
    , [binOpL "&" OpAnd]
    , [binOpL "|" OpOr]
    , [ binOpN "==" OpE
      , binOpN "!=" OpNE
      , binOpN ">=" OpGE
      , binOpN "<=" OpLE
      , binOpN "<"  OpL
      , binOpN ">"  OpG
      ]
    , [unop "!" OpLogicalNot]
    , [binOpL "&&" OpLogicalAnd]
    , [binOpL "||" OpLogicalOr]
    ]
  unop name op = Prefix (UnOpExpr <$> (op <$ lexeme (string name)))
  binOpL name op = InfixL (BinOpExpr <$> (op <$ lexeme (string name)))
  binOpN name op = InfixN (BinOpExpr <$> (op <$ lexeme (string name)))

statements :: Parser [Statement]
statements = statement `sepBy` lexeme ";"

statement :: Parser Statement
statement = choice
  [ Break <$> (lexeme "break" *> address)
  , Watch <$> (lexeme "watch" *> expression)
  , Run <$> (lexeme "run" *> optional expression)
  , Step <$> (lexeme "step" *> optional number)
  , StepOver <$> (lexeme "over" *> optional number)
  , StepOut <$> (lexeme "out" *> optional number)
  , Exit <$> (lexeme "exit" *> optional number)
  , ifStatement
  , lexeme "print" *> choice [Print <$> stringLiteral, PrintExpr <$> expression]
  , do
    expr <- expression
    choice [Set expr <$> (lexeme "=" *> expression), pure (Get expr)]
  ]
 where
  braces      = between (lexeme "{") (lexeme "}")
  ifStatement = If <$> (lexeme "if" *> expression) <*> braces statements <*> optional
    ((lexeme "elif" *> (pure <$> ifStatement)) <|> (lexeme "else" *> braces statements))
