module Pancakes where

import Data.List


getPanCakes :: String -> String
getPanCakes xs =
  let isEndHappy = xs !! ((length xs) -1) == '+'
      diffCount = length . group $ xs
  in if isEndHappy then show (diffCount -1) else show diffCount
  




