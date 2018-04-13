module QR.TripletSort where

import Data.List
import Control.Monad

getResult :: [String] -> [String]
getResult xs =
  let count = read (head xs)::Int
      lists = map (\a -> read a::Int) $ take count $ words (last xs)
      wrongIndex = getWrongInex $ getTripletSort lists
  in
    if wrongIndex >= 0
    then [show wrongIndex]
    else ["OK"]



getTripletSort :: [Int] -> [Int]
getTripletSort xs =
  let
    (ys,zs) = foldr (\(i,x) (as,bs) -> if odd i then (x:as, bs) else (as,x:bs)) ([],[]) $ zip [1..] xs
    ys' = sort ys
    zs' = sort zs
  in
    mergeList ys' zs' []


mergeList :: [Int] -> [Int] -> [Int] -> [Int]
mergeList [] _ zs = reverse zs
mergeList xs [] zs = reverse $ (reverse xs) ++ zs
mergeList (x:xs) (y:ys) zs = mergeList xs ys (y:x:zs)


getWrongInex :: [Int] -> Int
getWrongInex xs = fst $ foldl (\(i,a) (j,b)-> if i >= 0 then (i, a) else if a > b then ((j-1),b) else (i,b)) ((-1), (head xs)) $ zip [0..] xs

lineReadAndOutput :: IO([String])
lineReadAndOutput = do
    line1 <- getLine
    line2 <- getLine
    return [line1,line2]

caseOutput :: (Int, [String]) -> IO()
caseOutput (x, (y:ys)) = do
  putStrLn ("Case #" ++ (show x) ++ ": " ++ y)


main = do  
    count <- getLine
    let caseCount = read count::Int
    lines <- replicateM caseCount lineReadAndOutput
    let caseDatas = zip [1..] (map getResult lines)
    mapM_ caseOutput caseDatas
