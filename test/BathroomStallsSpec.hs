{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module BathroomStallsSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import BathroomStalls


spec :: Spec
spec = do
  describe "test getSleepNumber" $ do
    it "test getBathroomStalls" $
      fmap getBathroomStalls ["4 2", "5 2", "6 2","1000 1000", "1000 1"] `shouldBe` ["1 0","1 0","1 1","0 0", "500 499"]
