module TidyNumbers where

import Data.List
import Data.Char

getMaxTidyNumber :: String -> String
getMaxTidyNumber xs =
  let ys = fmap digitToInt xs
      maxNumber = maxTidyNumber ys
  in show maxNumber

maxTidyNumber:: [Int] -> Int
--maxTidyNumber x = if ysSize == 0 then intArrToInt xs else ((intArrToInt xs) - minusNumber) * (10 ^ ysSize) + (intArrToInt (replicate ysSize 9))
maxTidyNumber x = if ysSize == 0 then intArrToInt xs else ((intArrToInt xs) - minusNumber) * (10 ^ ysSize) + (intArrToInt (replicate ysSize 9))
  where
    (xs, ys) = foldl (\(a,b) y -> if (length a == 0  || ((last a) <= y)) && (length b ==0) then (a++[y],b) else (a,b++[y])) ([],[]) x
    lastXs = last xs
    ysSize = length ys
    lastSamNumberCount = length $ last (group xs)
--    minusNumber = if lastSamNumberCount == 1 then 1 else (intArrToInt ((replicate (lastSamNumberCount-2) lastXs)++[lastXs+1]))
    minusNumber = if lastSamNumberCount == 1 then 1 else (intArrToInt (replicate (lastSamNumberCount - 1) lastXs))+1

intArrToInt:: [Int] -> Int
intArrToInt [] = 0
intArrToInt xs = read (concat $ fmap show xs) ::Int
