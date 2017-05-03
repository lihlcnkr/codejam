{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module R1.ParentPartnerSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import R1.ParentPartner
import CodeJam


spec :: Spec
spec = do
  describe "test AmpleSyrup" $ do
    it "test AmpleSyrup1" $
      (codeJamCalc (ParentPartner ["1 1", "540 600","840 900"])) `shouldBe` "2"
    it "test 2" $
      (codeJamCalc (ParentPartner ["2 2", "900 1260","180 540"])) `shouldBe` "4"
    it "test 3" $
      (codeJamCalc (ParentPartner ["1 1", "1439 1440", "0 1"])) `shouldBe` "2"
    it "test 4" $
      (codeJamCalc (ParentPartner ["2 2", "0 1", "1439 1440", "1438 1439", "1 2"])) `shouldBe` "4"
    it "test5" $
      (codeJamCalc (ParentPartner ["3 4", "0 10", "1420 1440", "90 100", "550 600", "900 950", "100 150", "1050 1400"])) `shouldBe` "6"
