{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module R1.AmpleSyrupSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import R1.AmpleSyrup


spec :: Spec
spec = do
  describe "test AmpleSyrup" $ do
    it "test AmpleSyrup1" $
--     getAmpleSyrup 1 [(100.0,20.0),(200.0,10.0)] `shouldBe` 138230.076757951
     getCoinJam (["2 1", "100 20","200 10"]) `shouldBe` "138230.076757951"
    it "test 2" $
     getCoinJam (["2 2", "100 20","200 10"]) `shouldBe` "150796.447372310"
    it "test 3" $
     getCoinJam (["3 2", "100 10","100 10", "100 10"]) `shouldBe` "43982.297150257"
    it "test 4" $
      getCoinJam (["4 2", "9 3", "7 1", "10 1", "8 4"]) `shouldBe` "625.176938064"
--    it "test 5" $
--      getCoinJam ("3 2", ["5 1", "4 1", "3 1000"]) `shouldBe` "3000"
