module CoinJam where

import Data.List

import Data.Set (Set) 
import qualified Data.Set as Set


data PrimeRepo = PrimeRepo (primeSet::Set Int, maxChecked::Int) deriving Show


getCoinJam :: String -> String
getCoinJam x =
  let isEndHappy = xs !! ((length xs) -1) == '+'
      diffCount = length . group $ xs
  in if isEndHappy then show (diffCount -1) else show diffCount



isCoinJam :: [Int] -> Maybe [Int]
isCoinJam xs =




isPrim :: PrimeRepo -> Int -> Mabye Int
isPrim s x =
  let maxCheckedNum = maxChecked s
      sets = primeSet s
   in if maxCheckedNum >= x then 




