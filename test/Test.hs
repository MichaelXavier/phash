{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit
import Test.SmallCheck.Series

import Data.PHash

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [imageTests, hammingTests]

imageTests :: TestTree
imageTests = testGroup "imageHash" [
  testCase "file missing" $ do
    result <- imageHash "bogus"
    result @?= Nothing,
  testCase "file exists" $ do
    result <- imageHash "test/fixtures/grump.jpg"
    result @?= Just (PHash 17549625427362946731)
  ]

hammingTests :: TestTree
hammingTests = testGroup "hammingTests" [
  testProperty "hammingDistance a a = 0" $
    \ph -> hammingDistance ph ph == 0,
  testProperty "hammingDistance a a+1 /= 0" $
    \ph -> hammingDistance ph (ph + 1) /= 0
  ]

instance Monad m => Serial m PHash where
  series = PHash . fromInteger <$> series
