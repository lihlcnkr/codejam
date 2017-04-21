{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module TidyNumbersSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import TidyNumbers


spec :: Spec
spec = do
  describe "test getSleepNumber" $ do
    it "test getSleepNumber" $
      fmap getMaxTidyNumber ["132", "1000", "7","111111111111111110", "14458999373"] `shouldBe` ["129","999","7","99999999999999999", "14458899999"]
