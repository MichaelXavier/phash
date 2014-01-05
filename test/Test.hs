import Test.Tasty
import Test.Tasty.HUnit

import Data.PHash

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [phashTests]

phashTests :: TestTree
phashTests = testGroup "phash" [
  testCase "file missing" $ do
    result <- imageHash "bogus"
    result @?= Nothing
  ]
