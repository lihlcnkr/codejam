module TwiceLiner where



--import Data.Set (Set)
--import qualified Data.Set as Set
--data DblRecord = DblRecord {past::Int, datas::(Set Int)} deriving(Show)


import Data.List
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map

import Control.Monad.State.Lazy(State)
import qualified Control.Monad.State.Lazy as State

lexiPos :: String -> Integer
lexiPos xs =
 


lexiPos' :: String -> State (Map Char Int) Integer
lexiPos' (x:xs) = do
  sm <- get

 
 
  () +
 


totalLexiCount :: [Int] -> Integer
totalLexiCount [] = 0
totalLexiCount (0:xs) = totalLexiCount xs
totalLexiCount (x:xs) = (totalLexiCount xs) + (totalLexiCount [x-1]:xs)
 

toCharMap :: String -> Map Char Int
toCharMap ys = Map.fromList zs
  where
    zs = fmap (\xss@(x:xs) -> (x, length xss)) $ group $ sort ys



getSmallCharMap :: Char -> State (Map Char Int) (Map Char Int)
getSmallCharMap x = do
  sm <- State.get
  return $ filter (\y -> compare y x == LT) sm
 


getPossibleCount::[Int] -> Integer
getPossibleCount [] = 0
getPossibleCount xs = div (factorial (sum xs)) (product (fmap factorial xs))


factorial :: Int -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = (toInteger n) * (factorial (n-1))




