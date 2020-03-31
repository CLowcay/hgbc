module Debug.AST
  ( Value(..)
  , Address(..)
  , BinOp(..)
  , UnOp(..)
  , Expression(..)
  , Statement(..)
  , ExpandedRegister8(..)
  , ExpandedRegister16(..)
  )
where

import           Data.Word
import qualified Data.Text                     as T

data Address = ShortAddress Word16 | LongAddress Word16 Word16
  deriving (Eq, Ord, Show)

data Value
  = AddressValue Address
  | ByteValue Word8
  | WordValue Word16
  | IntValue Int
  | BoolValue Bool
  deriving (Eq, Ord, Show)

data BinOp
  = OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpShiftLeft
  | OpShiftRight
  | OpAnd
  | OpOr
  | OpLogicalAnd
  | OpLogicalOr
  | OpE
  | OpNE
  | OpLE
  | OpGE
  | OpL
  | OpG
  deriving (Eq, Ord, Show)

data UnOp
  = OpNot
  | OpLogicalNot
  deriving (Eq, Ord, Show)

data Expression
  = Register8Expr ExpandedRegister8
  | Register16Expr ExpandedRegister16
  | SymbolExpr T.Text
  | ValueExpr Value
  | UnOpExpr UnOp Expression
  | BinOpExpr BinOp Expression Expression
  | DereferenceExpr Expression
  | ParenthesizedExpr Expression
  deriving (Eq, Ord, Show)

data ExpandedRegister8
  = ReA
  | ReB
  | ReC
  | ReD
  | ReE
  | ReF
  | ReH
  | ReL
  deriving (Eq, Ord, Show)

data ExpandedRegister16
  = ReAF
  | ReBC
  | ReDE
  | ReHL
  | ReSP
  | RePC
  deriving (Eq, Ord, Show)

data Statement
  = Break Address
  | Watch Expression
  | Get Expression
  | Set Expression Expression
  | Run (Maybe Expression)
  | Step (Maybe Int)
  | StepOver (Maybe Int)
  | StepOut (Maybe Int)
  | Exit (Maybe Int)
  | Print T.Text
  | PrintExpr Expression
  | If Expression [Statement] (Maybe [Statement])
  deriving (Eq, Ord, Show)
