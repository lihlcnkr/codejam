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
    in sum (fmap (read) datas))


convertData :: String -> (Int, Int)
convertData x = (y, z)
  where datas = words x
        y = read (head datas)::Int
        z = read (last datas)::Int



getResult :: [(Int, Int)] -> [(Int, Int)] -> Int
getResult xs ys = 0
  where xs' = sortBy (\x y -> compare (fst x) (fst y)) xs
        ys' = sortBy (\x y -> compare (fst x) (fst y)) ys
        
  

type TotalLeftTime = Int
type TotalRightTime = Int
data LastTime = LeftTime Int | RightTime Int | Zero


mergeTimeData :: [(Int, Int)] -> [(Int, Int)] -> LastTime-> ([Int], [Int], TotalLeftTime, TotalRightTime)-> ([Int], [Int], TotalLeftTime, TotalRightTime)
mergeTimeData [] [] _ z = z
mergeTimeData [] (y:ys) lastTime (ax, bx, c, d) = case lastTime of
                                                    LeftTime leftTime -> mergeTimeData [] ys (Right (snd y)) (ax, bx, c, d+((snd y) - (fst y)))
                                                    RightTime rightTime -> mergeTimeData [] ys  (Right (snd y)) (ax,  insertBy (\x y -> compare y x) () bx, c , d+ ((snd y) - rightTime))
                                                    Zero  -> mergeTimeData [] ys  (Right (snd y)) (ax,  insertBy (\x y -> compare y x) (fst y) bx, c , d+ (snd y))


  
