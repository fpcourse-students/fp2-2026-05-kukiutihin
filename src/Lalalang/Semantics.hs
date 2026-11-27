module Lalalang.Semantics where

import Control.Applicative (asum)
import Data.Foldable (fold)
import Control.Monad (zipWithM, ap)
import Control.Monad.State
import Data.Map (Map, (!?))
import Data.Map qualified as Map

import Lalalang.Syntax

import MetaUtils

-- | Значения в Lalalang.
data LaValue
  = LaUnitValue
  | LaNum !Int
  deriving Eq

instance Show LaValue where
  show = \case
    LaUnitValue -> "unit"
    LaNum value -> show value

-- | Состояние программы - это просто словарь из имён переменных в их значения.
-- Ознакомьтесь с интерфейсом Data.Map с помощью Hoogle.
type LaEnv = Map LaName LaValue

newtype Domain val = Domain { getDomain :: LaEnv -> (LaEnv, val) }
  deriving stock Functor

instance Applicative Domain where
  pure val = Domain \env -> (env, val)
  (<*>) = ap

instance Monad Domain where
  Domain d >>= k = Domain \env ->
    case d env of
      (env', x) -> getDomain (k x) env'

instance MonadState LaEnv Domain where
  get = Domain \env -> (env, env)
  put newEnv = Domain (const (newEnv, ()))

eval :: LaExpr -> LaValue
eval expr = snd $ getDomain (evalLaExpr expr) Map.empty

evalWithInput :: LaExpr -> (Int -> Int)
evalWithInput expr input =
  let env = Map.singleton "input" (LaNum input) in
  unpackNum $ snd $ getDomain (evalLaExpr expr) env

-- | Интерпретатор выражений.
evalLaExpr :: LaExpr -> Domain LaValue
evalLaExpr expr = case expr of
  LaUnit -> pure LaUnitValue
  LaConst value -> pure (LaNum value)
  LaVar name -> get >>= \env -> case env !? name of
    Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
    Just value -> pure value
  LaBinOp op lhs rhs -> evalLaBinOp op <$> evalLaExpr lhs <*> evalLaExpr rhs
  LaIf c t e -> do
    n <- evalLaExpr c
    evalLaExpr $ if unpackNum n /= 0 then t else e

-- | Интерпретатор бинарных операций.
evalLaBinOp :: LaBinOp -> LaValue -> LaValue -> LaValue
evalLaBinOp op = case op of
  LaPlus -> wrapNum (+)
  LaMinus -> wrapNum (-)
  LaMult -> wrapNum (*)
  LaDiv -> wrapNum div
  LaLt -> wrapBool (<)
  LaLe -> wrapBool (<=)
  LaEq -> wrapBool (==)
  where
    wrapNum op lhs rhs = LaNum $ unpackNum lhs `op` unpackNum rhs
    wrapBool op lhs rhs = if unpackNum lhs `op` unpackNum rhs then LaNum 1 else LaNum 0

unpackNum :: LaValue -> Int
unpackNum = \case
  LaNum value -> value
  other -> error $ "TYPE_ERROR: Expected number, got " <> show other
