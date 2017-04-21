module Pancakes where

import Data.List


type SleepCount = Either String Int

getCodejam :: String -> IO String
getCodejam xs = do
  putStrLn xs
  let out = getPanCakes xs
  putStrLn out
  return out

getPanCakes :: String -> String
getPanCakes xs =
  let inputs = words xs
      ys = head inputs
      y = read (last inputs)::Int
  in
    either id show (pancakeHappyNumber ys y 0)
    --either id show (pancakeHappyNumber "+++++" 4 0)


-- getPanCakesNumber:: String -> Int -> SleepCount
-- getPanCakesNumber xs n =
--   case mod unhappyCount n of 0 -> Right (pancakeHappyNumber xs n 0)
--                              _ -> Left "IMPOSSIBLE"
--   where
--     unhappyCount = length $ filter (== '-') xs

pancakeHappyNumber:: String -> Int -> Int -> SleepCount
pancakeHappyNumber xs x y =
  let unHappyCake = dropWhileEnd (== '+') $ dropWhile (== '+') xs
      mHappyIndex = elemIndex '+' unHappyCake
   in if (length unHappyCake) == 0
      then Right y
      else if length unHappyCake < x
           then Left "IMPOSSIBLE"
           else case mHappyIndex of Nothing ->
                                      if mod (length unHappyCake) x == 0 then Right (y + (div (length unHappyCake) x)) else Left "IMPOSSIBLE"
                                    Just n -> case mod n x of 0 -> pancakeHappyNumber (drop n unHappyCake) x (y+(div n x))
                                                              m -> pancakeHappyNumber (flipPancake (drop (n-m) unHappyCake) x) x (y+(div n x)+1)


flipPancake :: String -> Int -> String
flipPancake xs x = 
  let (ys, zs) = splitAt x xs
      yxs = fmap flipCake ys
   in yxs ++ zs


flipCake:: Char -> Char
flipCake '+' = '-'
flipCake _ = '+'
