{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module QR.PrimeSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import Prime


spec :: Spec
spec = do
  describe "test prime" $ do
    it "test floorSqrt 9 -> 3" $
      floorSqrt 9 `shouldBe` 3

    it "test floorSqrt 17 -> 4" $
      floorSqrt 17 `shouldBe` 4

    it "test flooSqrt 37 -> 5" $
      floorSqrt 34 `shouldBe` 5
    it "testState 3" $
      testState 3 `shouldBe` ["3","4","5"]

