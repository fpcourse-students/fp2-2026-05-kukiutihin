module SpecLevel1 (tests, expectedFac) where

import Level1 qualified as F

import Control.DeepSeq
import Control.Exception
import Lalalang.Syntax
import Lalalang.Embedding
import Lalalang.Semantics
import Lalalang.Programs
import Test.Prelude

-- Скобки отделяют тело лямбды Lalalang от её применения.
{- HLINT ignore testFunctions "Move brackets to avoid $" -}
{- HLINT ignore testFunctions "Redundant bracket" -}
testFunctions :: Test
testFunctions = TestList
  [ TestCase $ assertEqual "function smoke" (LaNum 42) $
      eval $ (lam #x $ 1 .+ #x) @ 41
  , TestCase $ assertEqual "definition and call environments" (LaNum 110) $
      eval $ (lam #f ((lam #x (#f @ #x)) @ 100)) @
        ((lam #x (lam #y (#x + #y))) @ 10)
  , TestCase $ assertEqual "negation in a function" (LaNum (-3)) $ eval (lam #x (negate #x) @ 3)
  , TestCase $ assertThrows @ErrorCall $ eval (1 @ 2)
  , TestCase $ assertThrows @ErrorCall $ eval (lam #x 1 @ #missing)
  , TestCase $ assertEqual "lexical bindings" (LaNum 42) $
      eval $ (lam #x $ lam #y $ #x .+ #y) @ 1 @ 41
  , TestCase $ assertThrows @ErrorCall $
      eval $ (lam #x $ lam #y #y) @ 1 @ #x
  , TestCase $ assertEqual "closure" (LaNum 42) $
      eval $ lam #x (lam #x (lam #_ #x) @ 42) @ 100500 @ 0
  , TestCase $ assertEqual "rebind free" (LaNum 42) $
      eval $ (lam #x $ lam #x #x) @ 1 @ 42
  , TestCase $ assertEqual "displacement works" (LaNum 400) $
      eval $ displacement @ 50 @ 30 @ 2
  , propertyToTest "functional factorial" \(NonNegative (Small n)) ->
      eval (fac @ c n) === LaNum (expectedFac n)
  ]

deriving stock instance Generic LaValue
instance NFData LaBinOp
instance NFData LaExpr
instance NFData LaValue

expectedFac :: Int -> Int
expectedFac n = product [1..n]

testDisplacement :: Test
testDisplacement = TestList
  [ TestCase $ assertEqual "displacement works" (400 :: Int) (F.displacement 50 30 2)
  , propertyToTest "displacement for several inputs" \(Small v) (Small v0) (Positive (Small a)) ->
      F.displacement v v0 a === (v * v - v0 * v0) `div` (2 * a)
  ]

testExprString :: Test
testExprString = TestList
  [ TestCase $ assertEqual "nested arithmetic" "((3 * 2) + 42)" (F.expr1 :: String)
  , TestCase $
      let expected = "(((50 * 50) - (20 * 20)) / (2 * 10))" in
      let actual = F.displacement 50 20 10 in
      assertEqual "expr string target works" expected actual
  ]

tests :: NamedTests
tests = nameTests 1 [testFunctions, TestList [testDisplacement, testExprString]]
