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
    result @?= Just (PHash 17549625427362946731),
  testCase "match same image" $ do
    result <- imagesSimilar "test/fixtures/grump.jpg" "test/fixtures/grump.jpg" 1
    result @?= Just True,
  testCase "match desaturated image" $ do
    result <- imagesSimilar "test/fixtures/grump.jpg" "test/fixtures/grump_gray.jpg" 1
    result @?= Just True,
  testCase "match brighter image" $ do
    result <- imagesSimilar "test/fixtures/grump.jpg" "test/fixtures/grump_bright.jpg" 2
    result @?= Just True,
  testCase "does not match brighter image with too-low threshold" $ do
    result <- imagesSimilar "test/fixtures/grump.jpg" "test/fixtures/grump_bright.jpg" 1
    result @?= Just False,
  testCase "does not match different image" $ do
    result <- imagesSimilar "test/fixtures/grump.jpg" "test/fixtures/grump_flip.jpg" 15
    result @?= Just False,
  testCase "Returns Nothing on error on one file" $ do
    result <- imagesSimilar "test/fixtures/grump.jpg" "bogus" 15
    result @?= Nothing,
  testCase "Returns Nothing on error on both files" $ do
    result <- imagesSimilar "bogus" "also_bogus" 15
    result @?= Nothing
  ]

hammingTests :: TestTree
hammingTests = testGroup "hammingTests" [
  testProperty "hammingDistance a a = 0" $
    \ph -> hammingDistance ph ph == 0,
  testProperty "hammingDistance a b where a /= b /= 0" $
    \ph ph' -> ph /= ph' ==> hammingDistance ph ph' /= 0
  ]

instance Monad m => Serial m PHash where
  series = PHash . fromInteger <$> series
