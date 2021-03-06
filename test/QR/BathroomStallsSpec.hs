{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module QR.BathroomStallsSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import QR.TripletSort


spec :: Spec
spec = do
  describe "test getSleepNumber" $ do
    it "test getBathroomStalls" $
      fmap getResult [["5","5 6 8 4 3"],["3", "8 9 7"]] `shouldBe` [["OK"],["1"]]
