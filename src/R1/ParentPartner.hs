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
  | c > 720 = e + (fst (foldl (\(a,b) y -> if b <= 0 then (a,b) else case y of
                                                                       Middle h -> (a+2, (b-h))
                                                                       Side i -> (a+1, (b-i))
                              ) (0, 1440-c) ax))
  | d > 720 = e + (fst (foldl (\(a,b) y -> if b <= 0 then (a,b) else case y of
                                                                       Middle h -> (a+2, (b-h))
                                                                       Side i -> (a+1, (b-i))
                              ) (0, 1440-d) bx))
  | c == 0 || d == 0 = e + 1
  | otherwise = e
  where xs' = sortBy (\x y -> compare (fst x) (fst y)) xs
        ys' = sortBy (\x y -> compare (fst x) (fst y)) ys
        (ax, bx, c, d, e) = mergeTimeData xs' ys' Zero ([],[],0,0,0)


type TotalLeftTime = Int
type TotalRightTime = Int
data LastTime = LeftTime Int | RightTime Int | Zero
data SideMiddle = Middle Int | Side Int deriving (Show)


mergeTimeData :: [(Int, Int)] -> [(Int, Int)] -> LastTime-> ([SideMiddle], [SideMiddle], TotalLeftTime, TotalRightTime, Int)-> ([SideMiddle], [SideMiddle], TotalLeftTime, TotalRightTime, Int)
mergeTimeData [] [] lastTime (ax, bx, c, d, e) = case lastTime of
                                                   LeftTime leftTime -> (insertBy orderSideMiddle (Side (1440 - leftTime)) ax, bx, c, d, e)
                                                   RightTime rightTime -> (ax, insertBy orderSideMiddle (Side (1440 - rightTime)) bx, c, d, e)
                                                   Zero -> ([],[],0,0,0)
mergeTimeData (x:xs) [] lastTime (ax, bx, c, d, e) = case lastTime of
                                                    LeftTime leftTime -> mergeTimeData xs [] (LeftTime (snd x)) (insertBy orderSideMiddle (Middle ((fst x) - leftTime)) ax, bx, c+((snd x) - leftTime), d, e)
                                                    RightTime rightTime -> mergeTimeData xs []  (LeftTime (snd x)) (ax,  bx, c + ((snd x) - (fst x)) , d, (e+1))
                                                    Zero  -> mergeTimeData xs []  (LeftTime (snd x)) (ax,  insertBy orderSideMiddle (Side (fst x)) bx, c + (snd x) , d, e+1)

mergeTimeData [] (y:ys) lastTime (ax, bx, c, d, e) = case lastTime of
                                                    LeftTime leftTime -> mergeTimeData [] ys (RightTime (snd y)) (ax, bx, c, d+((snd y) - (fst y)), e+1)
                                                    RightTime rightTime -> mergeTimeData [] ys  (RightTime (snd y)) (ax,  insertBy orderSideMiddle (Middle (fst y - rightTime)) bx, c , d+ ((snd y) - rightTime), e)
                                                    Zero  -> mergeTimeData [] ys  (RightTime (snd y)) (ax,  insertBy orderSideMiddle (Middle (fst y)) bx, c , d+ (snd y), e+1)
mergeTimeData (x:xs) (y:ys) lastTime (ax, bx, c, d, e) = case lastTime of
                                                    LeftTime leftTime -> if x > y
                                                                         then mergeTimeData (x:xs) ys (RightTime (snd y)) (ax, bx, c, d+((snd y) - (fst y)), e+1)
                                                                         else mergeTimeData xs (y:ys) (LeftTime (snd x)) (insertBy orderSideMiddle (Middle ((fst x) - leftTime)) ax, bx, c+((snd x) - leftTime), d, e)
                                                    RightTime rightTime -> if x > y
                                                                           then mergeTimeData (x:xs) ys  (RightTime (snd y)) (ax,  insertBy orderSideMiddle (Middle (fst y - rightTime)) bx, c , d+ ((snd y) - rightTime), e)
                                                                           else mergeTimeData xs (y:ys)  (LeftTime (snd x)) (ax,  bx, c + ((snd x) - (fst x)) , d, e+1)
                                                    Zero  -> if x > y
                                                             then mergeTimeData (x:xs) ys  (RightTime (snd y)) (ax,  insertBy orderSideMiddle (Side (fst y)) bx, c , d+ (snd y), e+1)
                                                             else mergeTimeData xs (y:ys)  (LeftTime (snd x)) (ax,  insertBy orderSideMiddle (Side (fst x)) bx, c + (snd x) , d,e+1)


orderSideMiddle :: SideMiddle -> SideMiddle -> Ordering
orderSideMiddle (Middle x) (Middle y) = compare y x
orderSideMiddle (Middle x) (Side y) = compare y x
orderSideMiddle (Side x) (Middle y) = compare y x
orderSideMiddle (Side x) (Side y) = compare y x
