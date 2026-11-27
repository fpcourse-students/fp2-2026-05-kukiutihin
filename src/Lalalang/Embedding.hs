{-# OPTIONS_GHC "-Wno-orphans" #-}

-- | Средствами Haskell можно сделать задание синтаксических
-- деревьев языка Lalalang более простым и привычным.
module Lalalang.Embedding where

import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

import Lalalang.Syntax
import MetaUtils

unit :: LaExpr
unit = LaUnit

c :: Int -> LaExpr
c = LaConst

v :: LaName -> LaExpr
v = LaVar

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./
infix 4 .<
infix 4 .<=
infix 4 .==
(.+), (.-), (.*), (./), (.<), (.<=), (.==) :: LaExpr -> LaExpr -> LaExpr
(.+) = LaBinOp LaPlus
(.-) = LaBinOp LaMinus
(.*) = LaBinOp LaMult
(./) = LaBinOp LaDiv
(.<) = LaBinOp LaLt
(.<=) = LaBinOp LaLe
(.==) = LaBinOp LaEq

instance Num LaExpr where
  (+) = LaBinOp LaPlus
  (-) = LaBinOp LaMinus
  (*) = LaBinOp LaMult
  negate e = LaConst (-1) * e
  abs = undefined
  signum = undefined
  fromInteger = LaConst . fromInteger

instance KnownSymbol name => IsLabel name LaName where
  fromLabel = symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name LaExpr where
  fromLabel = LaVar $ fromLabel @name

-- ; нельзя использовать как кастомный оператор.
infixl 1 .:
(.:) :: LaExpr -> LaExpr -> LaExpr
(.:) = todo "(.:)"

infix 2 .:=
(.:=) :: LaName -> LaExpr -> LaExpr
(.:=) = todo "(.:=)"

infixl 9 @
(@) :: LaExpr -> LaExpr -> LaExpr
(@) = todo "(@)"

lam :: LaName -> LaExpr -> LaExpr
lam = todo "lam"

if_ :: LaExpr -> LaExpr -> LaExpr -> LaExpr
if_ = LaIf

while :: LaExpr -> LaExpr -> LaExpr
while = todo "while"
