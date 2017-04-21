module Main where

import Data.Set

import BathroomStalls
import System.Environment

readCaseAndToOutput::  (String -> String) -> (Int,String) -> IO String
readCaseAndToOutput f (x, y) = do
  putStrLn ("Case #" ++ (show x) ++ ": " ++ y)
  let ret = f y
  putStrLn ("ret:" ++ ret)
  return ("Case #" ++ (show x) ++ ": " ++ ret)



readFileToCases:: String -> IO [(Int, String)]
readFileToCases x = do
  s <- readFile x
  let (x:xs) = lines s
      caseCount = read x::Int
      caseDatas = take caseCount xs
  return $ zip [1..] caseDatas

--  "resources/codejam.in" "resources/codejam.out"
main = do
  (arg1:argRest) <- getArgs
  caseDatas <- readFileToCases arg1
  --mapM_ putStrLn $ fmap snd caseDatas
  outputs <- mapM (readCaseAndToOutput getBathroomStalls) caseDatas
  --mapM_ putStrLn $ fmap outputs
  writeFile (head argRest) $ unlines outputs
