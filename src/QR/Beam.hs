module QR.Beam where

import Data.Char
import Data.Sequence(Seq)
import qualified Data.Sequence as S


getMinChange :: String -> String
getMinChange xs =
  let inputs = words xs
      totalDamage = read (head inputs)::Int
      program = last inputs
      shootCount = length $ filter (== 'S') program
  in
    if shootCount > totalDamage
    then "IMPOSSIBLE"
    else show (getChangeNum program totalDamage 0)

toIntArr :: String -> [Int]
toIntArr xs = map (\x -> if x=='C' then 1 else 0) xs

getTotalDamage :: String -> Int -> Int -> Int
getTotalDamage [] _ z = z
getTotalDamage (x:xs) y z = if (x == 'S')
                            then getTotalDamage xs y (z+y)
                            else getTotalDamage xs (y*2) z

getChangeNum :: String -> Int -> Int -> Int
getChangeNum xs y z =
  let totalDamage = getTotalDamage xs 1 0
  in if totalDamage > y then (getChangeNum (replaceFirst xs 'C' 'S') y (z+1))
       else z

replaceFirst :: String -> Char -> Char -> String
replaceFirst (x:x':xs) y z = if x == y && x' == z then z:y:xs
  else x:(replaceFirst (x':xs) y z)

