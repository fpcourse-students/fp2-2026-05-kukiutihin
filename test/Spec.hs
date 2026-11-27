import Lambda.Console (setupConsole)
import SpecLevel1 qualified
import SpecLevel2 qualified
import SpecLevel3 qualified
import Test.Prelude

main :: IO ()
main = do
  setupConsole
  testMain $ SpecLevel1.tests ++ SpecLevel2.tests ++ SpecLevel3.tests
