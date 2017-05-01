module Main where


import AmpleSyrup
import System.Environment

readCaseAndToOutput::  (String -> String) -> (Int,String) -> IO String
readCaseAndToOutput f (x, y) = do
  putStrLn ("Case #" ++ (show x) ++ ": " ++ y)
  let ret = f y
  putStrLn ("ret:" ++ ret)
  return ("Case #" ++ (show x) ++ ": " ++ ret)


median::[Int] -> Float
median xs = if odd lenXs then fromIntegral (xs !! (div (lenXs-1) 2)) else ((fromIntegral ((xs !! ((div lenXs 2) -1) ) + (xs !! (div lenXs 2)))) / 2.0)::Float
  where lenXs = length xs


readFileToCases:: String -> IO [(Int, String)]
readFileToCases x = do
  s <- readFile x
  let (x:xs) = lines s
      caseCount = read x::Int
      caseDatas = take caseCount xs
  return $ zip [1..] caseDatas


readFileToCasesV2:: String -> IO [(Int, (String, [String]))]
readFileToCasesV2 x = do
  s <- readFile x
  let (x:xs) = lines s
      caseCount = read x::Int
  return $ getDatas caseCount xs


readCaseAndToOutputV2 :: ((String, [String]) -> String) -> (Int, (String, [String])) -> IO String
readCaseAndToOutputV2 f (x, y) = do
  --putStrLn ("Case #" ++ (show x) ++ ": " ++ y)
  putStrLn ("Case #" ++ (show x))
  let ret = f y
  putStrLn ("ret:" ++ ret)
  return ("Case #" ++ (show x) ++ ": " ++ ret)

getDatas:: Int -> [String] -> [(Int, (String, [String]))]
getDatas x ys = getDataRec ys [] x 0 0 "" []

getNextCount :: String -> Int
getNextCount y = 
  let datas = words y
  in read (head datas)::Int

getDataRec:: [String] -> [(Int, (String, [String]))] -> Int -> Int -> Int -> String -> [String] -> [(Int, (String, [String]))]
getDataRec _ ys 0 0 _ _ _= ys
getDataRec (x:xs) ys z _ b [] d = getDataRec xs ys z (getNextCount x) (b+1) x []
getDataRec (x:xs) ys z 0 b c d = getDataRec xs (ys ++ [(b, (c, d))]) (z-1) (getNextCount x) (b+1) x []
getDataRec [] ys z 0 b c d = ys ++ [(b, (c, d))]
getDataRec (x:xs) ys z a b c dx = getDataRec xs ys z (a-1) b c (dx++[x])

--  "resources/codejam.in" "resources/codejam.out"
main = do
  (arg1:argRest) <- getArgs
  --caseDatas <- readFileToCases arg1
  --outputs <- mapM (readCaseAndToOutput getBathroomStalls) caseDatas
  --mapM_ putStrLn $ fmap snd caseDatas
  caseDatas <- readFileToCasesV2 arg1
  
  outputs <- mapM (readCaseAndToOutputV2 getCoinJam) caseDatas
  
  
  --mapM_ putStrLn $ fmap outputs
  writeFile (head argRest) $ unlines outputs
