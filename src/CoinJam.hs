module CoinJam where

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Prime
import Data.Maybe



getCoinJam :: String -> String
getCoinJam x = getCoinJamByNum y z
  where datas = words x
        y = read (head datas)::Int
        z = read (last datas)::Int

getCoinJamByNum :: Int -> Int -> String
getCoinJamByNum x y =
  let datas = fmap (\x -> (1:x)++[1]) $ getPosibleList (x-2)
      ss = Set.fromList [2,3,5,7,11,13,17,19]
      s = PrimeRepo {primeSet=ss, maxChecked=20}
      rets = take y $ filter (\x -> isJust (snd (snd x))) $ fmap (isCoinJam s) datas
  in show rets


--take 3 $ snd $ mapAccumL isCoinJam s $ getPosibleList 6

getPosibleList :: Int -> [[Int]]
getPosibleList x = fmap (\y -> (1:y)++[1]) (sequence $ replicate (x-2) [0,1])


isCoinJam :: PrimeRepo-> [Int] -> (PrimeRepo, (String, Maybe [Int]))
isCoinJam s xs =
  let ms = fmap (getPowerNumber xs) [2..10]
      (ss,ys) = mapAccumL getDivisorNum s ms
  in (ss, (concat (fmap show xs), sequence $ ys))

getDivisorNum:: PrimeRepo -> Int -> (PrimeRepo, Maybe Int)
getDivisorNum s x = (z, y)
  where (y,z) = runWithScope x s


getPowerNumber :: [Int] -> Int -> Int
getPowerNumber xs y = sum $ fmap (\(a,b) -> if b==0 then 0 else y ^ a) $ zip [0..(length xs + 1)] (reverse xs)






