module SpecLevel2 (tests) where

import Control.Exception (ArithException, ErrorCall)
import Level1
import Level2
import Lalalang.Embedding ( (.:), (.:=), (.==), (.<=), (@), if_, unit, while )
import Lalalang.Embedding qualified as L
import Lalalang.Semantics (LaValue (..), eval)
import Lalalang.Programs (fac')
import SpecLevel1 (expectedFac)
import Test.Prelude

testControl :: Test
testControl = TestList
  [ TestCase $ assertEqual "zero chooses else" (7 :: Int) $ if' 0 9 7
  , TestCase $ assertEqual "negative chooses then" (9 :: Int) $ if' (-1) 9 7
  , TestCase $ assertEqual "first displacement is larger" (210 :: Int) $ maxDisplacement first second
  , TestCase $ assertEqual "second displacement is larger" (210 :: Int) $ maxDisplacement second first
  , TestCase $ do
      let left = displacement 50 20 5 :: String
          right = displacement 100 10 30 :: String
          printed = maxDisplacement first second
      assertBool ("expected a conditional choosing the larger displacement, got " ++ printed) $
        printed `elem` [if' (left .< right) right left, if' (right .< left) left right]
  , TestCase $ assertEqual "print control" "(if (1 < 2) then 3 else 4)" $ if' (c 1 .< c 2) (c 3) (c 4)
  ]
  where
    first = (50, 20, 5)
    second = (100, 10, 30)

-- Две лямбды явно показывают оба параметра каррированной функции.
{- HLINT ignore testTerm "Avoid lambda" -}
testTerm :: Test
testTerm = TestList
  [ TestCase $ assertEqual "compute works" 42 exampleCompute
  , TestCase $ assertEqual "returned function" 37 $ compute (app (lam \x -> lam \y -> plus x y) (val (30 :: Int))) 7
  ]

testTyped :: Test
testTyped = TestList
  [ TestCase $ assertEqual "integer" 42 $
      evalTyped (If (Less (Number 1) (Number 2))
        (Add (Number 20) (Number 22)) (error "unused branch"))
  , TestCase $ assertEqual "boolean" False $
      evalTyped (If (Boolean False) (error "unused branch") (Less (Number 2) (Number 2)))
  ]

testTree :: Test
testTree = TestList
  [ TestCase $ assertEqual "literal structure"
      (Sum (Product (Lit 3) (Lit 2)) (Lit 42)) expr1
  , TestCase $ assertEqual "comparison" (LessThan (Lit 2) (Lit 3)) (c 2 .< c 3)
  , TestCase $ assertEqual "build displacement"
      (Quotient (Difference (Product (Lit 7) (Lit 7)) (Product (Lit 1) (Lit 1)))
                (Product (Lit 2) (Lit 3))) (displacement 7 1 3)
  , TestCase $ assertEqual "fold into Int" (1 :: Int) (foldTree tree)
  , TestCase $ assertEqual "fold into String" "((((2 + 6) * 3) / (5 - 1)) < 8)" (foldTree tree)
  ]
  where
    tree = LessThan (Quotient (Product (Sum (Lit 2) (Lit 6)) (Lit 3))
                              (Difference (Lit 5) (Lit 1))) (Lit 8)

testImperative :: Test
testImperative = TestList
  [ TestCase $ assertEqual "seq" (LaNum 2) $ eval (1 .: 2)
  , TestCase $ assertThrows @ErrorCall $ eval ((#x .:= #missing) .: 42)
  , TestCase $ assertThrows @ArithException $ eval ((1 L../ 0) .: 42)
  , TestCase $ assertEqual "x + y" (LaNum 42) $ eval $
      #x .:= 20 .:
      #y .:= 22 .:
      #x + #y
  , TestCase $ assertEqual "assignment associative" (LaNum 2) $ eval $
      #x .:= 1 .:
      (#x .:= 2 .:
      #x)
  , TestCase $ assertEqual "assign assign" (LaNum 42) $ eval $
      #x .:= (#y .:= 42 .: #y) .:
      #x
  , TestCase $ assertEqual "global state" (LaNum 42) $ eval $
      #x .:= (#y .:= 21 .: #y) .:
      #x + #y
  , TestCase $ assertEqual "closure keeps its snapshot" (LaNum 1) $ eval $
      #x .:= 1 .: #f .:= L.lam #y #x .: #x .:= 10 .: (#f @ 0)
  , TestCase $ assertEqual "only argument effects escape" (LaNum 2) $ eval $
      #x .:= 1 .: (L.lam #y (#x .:= 3) @ (#x .:= 2 .: 0)) .: #x
  , TestCase $ assertEqual "mutations" (LaNum 42) $ eval $
      #x .:= 1 .:
      #y .:= 2 .:
      #x .:= #x + 19 .:
      #y .:= #y + 20 .:
      #x + #y
  , TestCase $ assertEqual "redef" (LaNum 2) $ eval $
      #x .:= 1 .:
      #x .:= 2 .:
      #x
  , TestCase $ assertEqual "effects with binops" (LaNum 42) $ eval $
      #y .:= (#x .:= 22 .: 20) .:
      #x + #y
  , TestCase $ assertEqual "effects with if" (LaNum 42) $ eval $
      #y .:= if_ (#x .:= 22 .: #x) 20 unit .:
      #x + #y
  , TestCase $ assertEqual "if and assignment" (LaNum 42) $ eval $
      (#x .:= 1) .:
      if_ (#x .== 2) (#x .:= 3) (#x .:= 42) .:
      #x
  , TestCase $ assertEqual "if executes only the chosen branch" (LaNum 3) $ eval $
      (#x .:= 1) .:
      if_ (#x .== 1) (#x .:= 3) (#x .:= 42) .:
      #x
  ]

testWhile :: Test
testWhile = TestList
  [ TestCase $ assertEqual "while (0)" (LaNum 42) $ eval $
      (#x .:= 42) .:
      (#cond .:= 0) .:
      while #cond (#x .:= 1) .:
      #x
  , TestCase $ assertEqual "while (...)" (LaNum 55) $ eval $
      (#res .:= 0) .:
      (#i .:= 0) .:
      while (#i .<= 10) (
        (#res .:= #res + #i) .:
        (#i .:= #i + 1)
      ) .:
      #res
  , TestCase $ assertEqual "effects with while" (LaNum 42) $ eval $
      while (#x .:= 42 .: 0) unit .:
      #x
  , propertyToTest "imperative factorial" \(NonNegative (Small n)) ->
      fac' n === expectedFac n
  ]

tests :: NamedTests
tests = nameTests 2 [testTyped, testControl, testTree, testTerm, TestList [testImperative, testWhile]]
