{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module PanCakesSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import Sleep


spec :: Spec
spec = do
  describe "test getPancakes " $ do
    it "test getSleepNumber" $
      getPanCakes "-" `shouldBe` "1"

    it "test getSleepNumber" $
      getPanCakes "-+" `shouldBe` "1"

    it "test getSleepNumber" $
      getPanCakes "+-" `shouldBe` "2"

    it "test getSleepNumber" $
      getPanCakes "+++" `shouldBe` "0"
    it "test getSleepNumber" $
      getPanCakes "--+-" `shouldBe` "3"
