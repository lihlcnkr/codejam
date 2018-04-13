module CodeWars.BraceExpansion where

import Debug.Trace

debug = flip trace

expandBraces :: String -> [String]
expandBraces "" = [""] 
expandBraces x = foldr foldExpand [""] $ map evalExpand $ parseExpandStr x 0 "" [] `debug` x

data BraceStr = BraceExp [String]
         | Normal String deriving (Show)

parseExpandStr :: String -> Int -> String-> [BraceStr] -> [BraceStr]
parseExpandStr [] _ "" zs = zs
parseExpandStr [] _ ys zs = ((Normal (reverse ys)):zs) 
parseExpandStr ('{':xs) 0 [] zs = parseExpandStr xs 1 "" zs
parseExpandStr ('{':xs) level ys zs = if level ==0
  then
  parseExpandStr xs (level +1) "" ((Normal (reverse ys)):zs)
  else
  parseExpandStr xs (level+1) ('{':ys) zs
parseExpandStr ('}':xs) level ys zs = if level == 1
  then
  parseExpandStr xs (level - 1) "" ((BraceExp (split (reverse ys) ',')):zs)
  else
  parseExpandStr xs (level - 1) ('}':ys) zs
parseExpandStr (x:xs) level ys zs = parseExpandStr xs level (x:ys) zs

evalExpand :: BraceStr -> [String]
evalExpand (BraceExp xs) = mconcat $ fmap expandBraces xs
evalExpand (Normal x) = if elem '{' x
                        then expandBraces x
                        else [x]

foldExpand :: [String] -> [String] -> [String]
foldExpand ys xs = do
  x <- xs
  y <- ys
  return (x ++ y)


split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) y = reverse zs
  where zs = splitNotBrace (x:xs) 0 y [] [] `debug` ('!':(x:xs))
  -- where zs = split' (x:xs) y [] []

split' :: String -> Char -> String -> [String] -> [String]
split' [] _ zs result = (reverse zs):result
split' (x:xs) y zs result = if x == y
                            then split' xs y [] ((reverse zs):result)
                            else split' xs y (x:zs) result


splitNotBrace :: String -> Int -> Char -> String -> [String] -> [String]
splitNotBrace []  _ _ zs result = (reverse zs):result
splitNotBrace ('{':xs) level y zs result = splitNotBrace xs (level+1) y ('{':zs) result
splitNotBrace ('}':xs) level y zs result = splitNotBrace xs (level -1) y ('}':zs) result 
splitNotBrace (x:xs) 0 y zs result = if x == y
  then splitNotBrace xs 0 y [] ((reverse zs):result)
  else splitNotBrace xs 0 y (x:zs) result
splitNotBrace (x:xs) level y zs result =  splitNotBrace xs level y (x:zs) result
