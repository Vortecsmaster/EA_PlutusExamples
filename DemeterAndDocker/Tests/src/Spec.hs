module Spec where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- Property: Reversing twice is identity
prop_reverse :: Property
prop_reverse = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) genInt
  reverse (reverse xs) === xs


genInt :: Gen Int
genInt = Gen.int (Range.linear 1 100)


tests :: TestTree
tests =
  testGroup "Property Tests"
    [ testProperty "Reversing twice is identity" prop_reverse
    ]