module CodeWars.TwiceLiner where



--import Data.Set (Set)
--import qualified Data.Set as Set
--data DblRecord = DblRecord {past::Int, datas::(Set Int)} deriving(Show)


import Data.List
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set


--import Control.Monad.State.Lazy(State)
--import qualified Control.Monad.State.Lazy as State

lexiPos :: String -> Integer
lexiPos xs = lexiPos' xs xsm
  where xsm = toCharMap xs
 


lexiPos' :: String -> (Map Char Int) -> Integer
lexiPos' [] _ = 0
lexiPos' (x:[]) _ = 1
lexiPos' (x:xs) ms = (lexiPos' xs (minusChar ms x)) + (sum (fmap getPossibleCount (getSmallCharList x ms)))


minusChar :: (Map Char Int) -> Char -> (Map Char Int)
minusChar xm y = Map.update  (\a -> if a > 1 then Just (a-1) else Nothing) y xm


toCharMap :: String -> (Map Char Int)
toCharMap ys = Map.fromList zs
  where
    zs = fmap (\xss@(x:xs) -> (x, length xss)) $ group $ sort ys



getSmallCharList :: Char -> (Map Char Int) -> [[Int]]
getSmallCharList x ym = fmap (\k -> (Map.elems . (minusChar ym)) k) smallKeys
  where keys = Map.keysSet ym
        smallKeys = Set.toList $ Set.filter (\k -> compare k x == LT) keys
  

getPossibleCount::[Int] -> Integer
getPossibleCount [] = 0
getPossibleCount xs = div (factorial (sum xs)) (product (fmap factorial xs))


factorial :: Int -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = (toInteger n) * (factorial (n-1))




