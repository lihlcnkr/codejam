module R1.ParentPartner where

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

import CodeJam



newtype ParentPartner = ParentPartner [String]

instance CodeJam ParentPartner where
  codeJamCalc (ParentPartner (x:ys)) = show $ getResult ys'' zs''
    where datas = words x
          y = read (head datas)::Int
          z = read (last datas)::Int
          (ys', zs') = splitAt y ys
          ys'' = fmap convertData ys'
          zs'' = fmap convertData zs'


  getData (ParentPartner xs) = xs

  toCodeJam xs = ParentPartner xs


getTotalLineCount :: String -> Int
getTotalLineCount x =
    let datas = words x
    in sum (fmap (read) datas)


convertData :: String -> (Int, Int)
convertData x = (y, z)
  where datas = words x
        y = read (head datas)::Int
        z = read (last datas)::Int



getResult :: [(Int, Int)] -> [(Int, Int)] -> Int
getResult xs ys
  | c > 720 = e + (fst (foldl (\(a,b) y -> if b <= 0 then (a,b) else (a+2, (b-y))) (0, c-720) ax'))
  | d > 720 = e + (fst (foldl (\(a,b) y -> if b <= 0 then (a,b) else (a+2, (b-y))) (0, d-720) bx'))
  | c == 0 || d == 0 = e + 2
  | otherwise = e
  where xs' = sortBy (\x y -> compare (fst x) (fst y)) xs
        ys' = sortBy (\x y -> compare (fst x) (fst y)) ys
        (ax, bx, c, d, e) = mergeData xs' ys' 1440 0 1440 0 ([],[],0,0,1)
        ax' = sortBy (flip compare) ax
        bx' = sortBy (flip compare) bx


type TotalLeftTime = Int
type TotalRightTime = Int

mergeData :: [(Int,Int)] -> [(Int, Int)] -> Int -> Int -> Int -> Int -> ([Int], [Int], TotalLeftTime, TotalRightTime, Int) -> ([Int], [Int], TotalLeftTime, TotalRightTime, Int)
mergeData [] [] lMin lMax rMin rMax (lx, rx, lt, rt, e)
  | lMin < rMin && lMax > rMax = (insert (1440 - lMax + lMin) lx, rx,lt + (1440 - lMax),rt , e-1)
  | lMin > rMin && lMax < rMax = (lx, insert (1440 - rMax + rMin) rx, lt, rt+(1440 - rMax), e -1)
  | lMin < rMin && lMax < rMax = (insert lMin lx, insert (1440 - rMax) rx, lt, rt, e)
  | otherwise = (insert (1440 - lMin) lx, insert rMin rx, lt, rt, e)

mergeData (x:xs) [] lMin lMax rMin rMax (lx, rx, lt, rt, e)
  | rMax > lMax = mergeData xs [] (min (fst x) lMin) (snd x)  rMin rMax (lx, rx, lt + (getDuration x), rt , e+1)
  | rMax == lMax = mergeData xs [] (min (fst x) lMin) (snd x)  rMin rMax (lx, rx, lt + (snd x) - lMax, rt , e)
  | otherwise = mergeData xs [] (min (fst x) lMin) (snd x)  rMin rMax (insert ((fst x) - lMax) lx, rx, lt + (snd x) - lMax, rt , e)
mergeData [] (y:ys) lMin lMax rMin rMax (lx, rx, lt, rt, e)
  | rMax < lMax = mergeData [] ys lMin lMax (min (fst y) rMin) (snd y) (lx, rx, lt, rt + (getDuration y) , e+1)
  | rMax == lMax = mergeData [] ys lMin lMax (min (fst y) rMin) (snd y) (lx, rx, lt,  rt + (snd y) - rMax, e)
  | otherwise = mergeData [] ys lMin lMax (min (fst y) rMin) (snd y) (lx, insert ((fst y) - rMax) rx, lt,  rt + (snd y) - rMax, e)
mergeData (x:xs) (y:ys) lMin lMax rMin rMax (lx, rx, lt, rt, e)
  | (fst x) < (fst y) && rMax > lMax = mergeData xs (y:ys) (min (fst x) lMin) (snd x)  rMin rMax (lx, rx, lt + (getDuration x), rt , e+1)
  | (fst x) < (fst y) && rMax <= lMax = mergeData xs (y:ys) (min (fst x) lMin) (snd x)  rMin rMax (insert ((snd x) - lMax) lx, rx, lt + (snd x) - lMax, rt , e)
  | (fst x) >= (fst y) && lMax > rMax = mergeData (x:xs) ys lMin lMax (min (fst y) rMin) (snd y) (lx, rx, lt, rt + (getDuration y) , e+1)
  | otherwise =  mergeData (x:xs) ys lMin lMax (min (fst y) rMin) (snd y) (lx, insert ((snd y) - rMax) rx, lt,  rt + (snd y) - rMax, e)

getDuration :: (Int,Int) -> Int
getDuration (x, y) = y - x
    
orderSideMiddle :: Int -> Int -> Ordering
orderSideMiddle = flip compare
