{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module QR.PanCakesSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import QR.Pancakes


spec :: Spec
spec = do
  describe "test getPancakes " $ do
    it "test getSleepNumber" $
      fmap getPanCakes ["---+-++- 3", "+++++ 4", "-+-+- 4", "+++---++ 2"] `shouldBe` ["3","0","IMPOSSIBLE", "IMPOSSIBLE"]
