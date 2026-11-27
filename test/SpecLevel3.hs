module SpecLevel3 (tests) where

import Level2 (Term (..))
import Level3
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.List (elemIndex)
import Text.ParserCombinators.ReadP qualified as P
import Test.Prelude

-- Сравниваем синтаксис с индексами связываний. Студент выбирает имена сам.
data TermShape
  = Literal Int
  | Bound Int
  | Lambda TermShape
  | Apply TermShape TermShape
  | Add TermShape TermShape
  deriving stock (Eq, Show)

token :: String -> P.ReadP String
token s = P.string s <* P.skipSpaces

identifier :: P.ReadP String
identifier = do
  first <- P.satisfy (\c -> isAscii c && isAlpha c)
  rest <- P.munch (\c -> isAscii c && isAlphaNum c)
  P.skipSpaces
  pure (first : rest)

shape :: [String] -> P.ReadP TermShape
shape env = literal P.<++ variable P.<++ P.between (token "(") (token ")") compound
  where
    literal = Literal <$> P.readS_to_P reads <* P.skipSpaces
    variable = do
      name <- identifier
      maybe P.pfail (pure . Bound) (elemIndex name env)
    compound = abstraction P.<++ binary
    abstraction = do
      _ <- token "\\"
      name <- identifier
      _ <- token "->"
      Lambda <$> shape (name : env)
    binary = do
      left <- shape env
      op <- (Add <$ token "+") P.<++ pure Apply
      op left <$> shape env

matches :: String -> TermShape -> String -> Test
matches label expected actual = TestCase $
  assertEqual (label ++ ": " ++ actual) [expected]
    [tree | (tree, _) <- P.readP_to_S (P.skipSpaces *> shape [] <* P.eof) actual]

tests :: NamedTests
tests = nameTests 3 [testPrinting]

-- Имена параметров показывают, какое связывание проверяет каждый пример.
{- HLINT ignore testPrinting "Use const" -}
{- HLINT ignore testPrinting "Avoid lambda" -}
testPrinting :: Test
testPrinting = TestList
  [ matches "example" (Apply (Lambda (Add (Bound 0) (Literal 41))) (Literal 1)) exampleShow
  , matches "outer" (Lambda (Lambda (Bound 1))) $ render (lam \x -> lam \_ -> x)
  , matches "inner" (Lambda (Lambda (Bound 0))) $ render (lam \_ -> lam id)
  , matches "both" (Lambda (Lambda (Add (Bound 1) (Bound 0)))) $
      render (lam \x -> lam \y -> plus x y)
  , matches "function argument" (Lambda (Apply (Bound 0) (Literal (-3)))) $
      render (lam \f -> app f (val (-3 :: Int)))
  ]
