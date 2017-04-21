module BathroomStalls where

import Data.List
import Data.Char
--import Data.SortedList(SortedList)
--import qualified Data.SortedList as S

getBathroomStalls :: String -> String
getBathroomStalls xs =
  let inputs = words xs
      ys = [read (head inputs)::Int]
      y = read (last inputs)::Int
      (maxLR,minLR) = stallLRSet ys y
  in show maxLR ++ " " ++ (show minLR)

stallLRSet:: [Int] -> Int -> (Int, Int)
stallLRSet xs y
  |y > length xs= stallLRSet (concat $ fmap splitStall xs) (y - (length xs))
  |otherwise = (z, head zs)
     where z:zs = splitStall ((sortBy (flip compare) xs) !! (y-1))
  
--  | y > length xs +1 = stallLRSet (sortBy (flip compare) $ concat $ fmap splitStall (x:xs)) (y - 1 - (length xs))
--  | otherwise = stallLRSet (splitStallAndPut x xs) (y -1)


splitStall::Int -> [Int]
splitStall x = (x -1 - halfX):halfX:[]
  where
    halfX = div (x-1) 2

splitStallAndPut::Int -> [Int] -> [Int]
splitStallAndPut x xs = insertBy (flip compare) leftX xsx
  where
    halfX = div (x-1) 2
    xsx = insertBy (flip compare) halfX xs
    leftX = x -1 - halfX

