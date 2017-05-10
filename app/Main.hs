module Main where

import CodeJam
import R1.ParentPartner
import System.Environment
import Data.List

readFileToCases:: String -> IO (Int, [String])
readFileToCases x = do
  s <- readFile x
  let (x:xs) = lines s
      caseCount = read x::Int
  return (caseCount,xs)




readCaseAndToOutputV2 :: (CodeJam a) => (a -> String) -> (Int, a) -> IO String
readCaseAndToOutputV2 f (x, y) = do
  --putStrLn ("Case #" ++ (show x) ++ ": " ++ y)
  --putStrLn ("Case #" ++ (show x) ++ ":" ++ (head (getData y)))
  putStrLn ("Case #" ++ (show x) ++ ":" ++ (concat (intersperse "," (getData y))))
  let ret = f y
  putStrLn ("ret:" ++ ret)
  return ("Case #" ++ (show x) ++ ": " ++ ret)



--  "resources/codejam.in" "resources/codejam.out"
main = do
  (arg1:argRest) <- getArgs
  --caseDatas <- readFileToCases arg1
  --outputs <- mapM (readCaseAndToOutput getBathroomStalls) caseDatas
  --mapM_ putStrLn $ fmap snd caseDatas
  (datCount, datas) <- readFileToCases arg1
  let caseDatas = zip [1..] $ ((splitDatas datas getTotalLineCount)::[ParentPartner])
  outputs <- mapM (readCaseAndToOutputV2 codeJamCalc) caseDatas
  
  --mapM_ putStrLn $ fmap outputs
  writeFile (head argRest) $ unlines outputs
