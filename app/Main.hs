module Main where

import Data.Set

import Sleep
import Pancakes
import System.Environment

readCaseAndToOutput::  (String -> String) -> (Int,String) -> String
readCaseAndToOutput f (x, y) = "Case #" ++ (show x) ++ ": " ++ (f y)



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
  let outputs = fmap (readCaseAndToOutput getPanCakes) caseDatas
  writeFile (head argRest) $ unlines outputs
