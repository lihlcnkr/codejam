{-# LANGUAGE QuasiQuotes
           , RecordWildCards #-}
module CodeWars.BranceExpansionSpec where
import Test.Hspec
import Data.List (sort)

import CodeWars.BraceExpansion


spec :: Spec
spec = do
    describe "Brace expansion" $ do
      it "Example tests" $ do
        sort (expandBraces "~/{Downloads,Pictures}/*.{jpg,gif,png}") `shouldBe` [ "~/Downloads/*.gif"
                                                                                , "~/Downloads/*.jpg"
                                                                                , "~/Downloads/*.png"
                                                                                , "~/Pictures/*.gif"
                                                                                , "~/Pictures/*.jpg"
                                                                                , "~/Pictures/*.png"
                                                                                ]
        sort (expandBraces "It{{em,alic}iz,erat}e{d,}, please.") `shouldBe` [ "Italicize, please."
                                                                            , "Italicized, please."
                                                                            , "Itemize, please."
                                                                            , "Itemized, please."
                                                                            , "Iterate, please."
                                                                            , "Iterated, please."
                                                                            ]
        sort (expandBraces "thumbnail.{png,jp{e,}g}") `shouldBe` [ "thumbnail.jpeg"
                                                                 , "thumbnail.jpg"
                                                                 , "thumbnail.png"
                                                                 ]
        sort (expandBraces "{p{{e,f,i}}g") `shouldBe` [ "peg", "pfg"
                                                                 , "pig"
                                                                 ]
        sort (expandBraces "{p{{e}a{b}}") `shouldBe` [ "peab"]
        sort (expandBraces "Ab{{uu,anod}ez,ezret}e{d,{a,b}{ariel{ex,edit},f}} folder.") `shouldBe` ["Abanodezeaarieledit folder.","Abanodezeaarielex folder.","Abanodezeaf folder.","Abanodezebarieledit folder.","Abanodezebarielex folder.","Abanodezebf folder.","Abanodezed folder.","Abezreteaarieledit folder.","Abezreteaarielex folder.","Abezreteaf folder.","Abezretebarieledit folder.","Abezretebarielex folder.","Abezretebf folder.","Abezreted folder.","Abuuezeaarieledit folder.","Abuuezeaarielex folder.","Abuuezeaf folder.","Abuuezebarieledit folder.","Abuuezebarielex folder.","Abuuezebf folder.","Abuuezed folder."]
        sort (expandBraces "nothing to do") `shouldBe` [ "nothing to do" ]
