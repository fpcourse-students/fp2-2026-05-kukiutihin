-- | Абстрактный синтаксис Lalalang.
module Lalalang.Syntax where

import Text.PrettyPrint.GenericPretty
import MetaUtils

-- | Перечень бинарных операций Lalalang.
data LaBinOp = LaPlus | LaMinus | LaMult | LaDiv | LaLt | LaLe | LaEq

-- | Синоним предметной области для типа имён переменных в Lalalang AST.
type LaName = String

-- | Деревья выражений Lalalang.
data LaExpr
  = LaUnit
  | LaConst Int
  | LaVar LaName
  | LaBinOp LaBinOp LaExpr LaExpr
  | LaIf LaExpr LaExpr LaExpr

deriving stock instance Eq LaBinOp
deriving stock instance Generic LaBinOp
instance Out LaBinOp
deriving via ShowOut LaBinOp instance Show LaBinOp

deriving stock instance Eq LaExpr
deriving stock instance Generic LaExpr
instance Out LaExpr
deriving via ShowOut LaExpr instance Show LaExpr
