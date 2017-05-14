{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module QR.SleepSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import QR.Sleep


spec :: Spec
spec = do
  describe "test getSleepNumber" $ do
    it "test getSleepNumber" $
      getSleepNumber 1 empty 1 1 `shouldBe` (Left 10)
    it "test getSleepNumbe 0r" $
      getSleepNumber 0 empty 0 1 `shouldBe` (Right "INSOMNIA")
    it "test getSleepNumbe 2" $
      getSleepNumber 2 empty 2 1 `shouldBe` (Left 90)
    it "test getSleepNumbe 11" $
      getSleepNumber 11 empty 11 1 `shouldBe` (Left 110)
    it "test getSleepNumbe 1692" $
      getSleepNumber 1692 empty 1692 1 `shouldBe` (Left 5076)
