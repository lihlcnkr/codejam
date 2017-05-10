{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module R1.ParentPartnerSpec where

import Test.Hspec
import Data.Maybe
import Data.Set
import R1.ParentPartner
import CodeJam
import Data.List


getMergeDataCount :: [(Int, Int)] -> [(Int, Int)] -> Int
getMergeDataCount xs ys =
  let
    xs' = sortBy (\x y -> compare (fst x) (fst y)) xs
    ys' = sortBy (\x y -> compare (fst x) (fst y)) ys
    (_, _, _, _, e) = mergeData xs' ys' 1440 0 1440 0 ([],[],0,0,1)
  in e


spec :: Spec
spec = do
  describe "mergeData" $ do
    it "merge 1" $
      getMergeDataCount [(540,600)] [(840,900)] `shouldBe` 2
    it "merge 1-1" $
      getMergeDataCount [(840,900)] [(540,600)] `shouldBe` 2
    it "merge 2" $
      getMergeDataCount [(180,540),(900, 1260)] [] `shouldBe` 0
    it "merge 2-1" $
      getMergeDataCount [] [(180,540),(900, 1260)] `shouldBe` 0
    it "merge 3" $
      getMergeDataCount [(1439,1440)] [(0,1)] `shouldBe` 2
    it "merge 3-1" $
      getMergeDataCount [(0,1)] [(1439,1440)] `shouldBe` 2
    it "merge 4" $
      getMergeDataCount [(0,1),(1439,1440)] [(1438,1439),(1,2)] `shouldBe` 2
    it "merge 4-1" $
      getMergeDataCount [(1438,1439),(1,2)] [(0,1),(1439,1440)] `shouldBe` 2
    it "merge 5" $
      getMergeDataCount [(0,10),(90,100),(1420,1440)] [(100,150), (900,950), (1050,1400)] `shouldBe` 2
    it "merge 5-1" $
      getMergeDataCount [(100,150), (900,950), (1050,1400)] [(0,10),(90,100),(1420,1440)] `shouldBe` 2

  describe "test AmpleSyrup" $ do
    it "test 1" $
      (codeJamCalc (ParentPartner ["1 1", "540 600","840 900"])) `shouldBe` "2"
    it "test 1-1" $
      (codeJamCalc (ParentPartner ["1 1","840 900",  "540 600"])) `shouldBe` "2"
    it "test 2" $
      (codeJamCalc (ParentPartner ["2 0", "900 1260","180 540"])) `shouldBe` "4"
    it "test 2-1" $
      (codeJamCalc (ParentPartner ["0 2","180 540", "900 1260"])) `shouldBe` "4"
    it "test 3" $
      (codeJamCalc (ParentPartner ["1 1", "1439 1440", "0 1"])) `shouldBe` "2"
    it "test 3-1" $
      (codeJamCalc (ParentPartner ["1 1", "0 1", "1439 1440"])) `shouldBe` "2"
    it "test 4" $
      (codeJamCalc (ParentPartner ["2 2", "0 1", "1439 1440", "1438 1439", "1 2"])) `shouldBe` "4"
    it "test 4-1" $
      (codeJamCalc (ParentPartner ["2 2", "1438 1439", "1 2", "0 1", "1439 1440"])) `shouldBe` "4"
    it "test5" $
      (codeJamCalc (ParentPartner ["3 4", "0 10", "1420 1440", "90 100", "550 600", "900 950", "100 150", "1050 1400"])) `shouldBe` "6"
    it "test5-1" $
      (codeJamCalc (ParentPartner ["4 3", "550 600", "900 950", "100 150", "1050 1400", "0 10", "1420 1440", "90 100"])) `shouldBe` "6"
