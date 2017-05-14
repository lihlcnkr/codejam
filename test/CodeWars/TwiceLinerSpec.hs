{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module CodeWars.TwiceLinerSpec where

import Test.Hspec
import CodeWars.TwiceLiner




spec :: Spec
spec = do
  describe "test lexiPos" $ do
    it "testing 'A'" $ shouldBe (lexiPos "A") 1    
    it "testing 'ABAB'" $ shouldBe (lexiPos "ABAB") 2   
    it "testing 'AAAB'" $ shouldBe (lexiPos "AAAB") 1   
    it "testing 'BAAA'" $ shouldBe (lexiPos "BAAA") 4
    it "testing 'QUESTION'" $ shouldBe (lexiPos "QUESTION") 24572
    it "testing 'BOOKKEEPER'" $ shouldBe (lexiPos "BOOKKEEPER") 10743
    it "testing 'IMMUNOELECTROPHORETICALLY'" $ shouldBe (lexiPos "IMMUNOELECTROPHORETICALLY") 718393983731145698173
