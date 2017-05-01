module Main where

import CodeJam
import AmpleSyrup
import System.Environment

readFileToCases:: String -> IO (Int, [String])
readFileToCases x = do
  s <- readFile x
  let (x:xs) = lines s
      caseCount = read x::Int
  return (caseCount,xs)




readCaseAndToOutputV2 :: ([String] -> String) -> (Int, [String]) -> IO String
readCaseAndToOutputV2 f (x, y) = do
  --putStrLn ("Case #" ++ (show x) ++ ": " ++ y)
  putStrLn ("Case #" ++ (show x) ++ ":" ++ (head y))
  let ret = f y
  putStrLn ("ret:" ++ ret)
  return ("Case #" ++ (show x) ++ ": " ++ ret)


getNextCount :: String -> Int
getNextCount y =
  let datas = words y
  in read (head datas)::Int




--  "resources/codejam.in" "resources/codejam.out"
main = do
  (arg1:argRest) <- getArgs
  --caseDatas <- readFileToCases arg1
  --outputs <- mapM (readCaseAndToOutput getBathroomStalls) caseDatas
  --mapM_ putStrLn $ fmap snd caseDatas
  (datCount, datas) <- readFileToCases arg1
  let caseDatas = zip [1..] $ splitDatas datas getNextCount
  outputs <- mapM (readCaseAndToOutputV2 (codeJamCalc . AmpleSyrup)) caseDatas
  
  --mapM_ putStrLn $ fmap outputs
  writeFile (head argRest) $ unlines outputs
