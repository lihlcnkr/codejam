module BathroomStalls where


import Data.Char
import Data.Sequence(Seq)
import qualified Data.Sequence as S

getBathroomStalls :: String -> String
getBathroomStalls xs =
  let inputs = words xs
      ys = [read (head inputs)::Int]
      y = read (last inputs)::Int
      (maxLR,minLR) = stallLRSet (S.fromList ys) y
  in show maxLR ++ " " ++ (show minLR)

stallLRSet:: Seq Int -> Int -> (Int, Int)
stallLRSet xs y
  |y <= fsSize= (retx, rety)
  |otherwise = stallLRSet (insertNtime fsSize rety (insertNtime fsSize retx ts)) (y - fsSize)
     where
       x = S.index xs 0
       (fs,ts) = S.breakl (/= x) xs
       (retx, rety) = splitStall x
       fsSize = length fs


insertNtime :: Ord a=> Int -> a -> Seq a -> Seq a
insertNtime n x (S.viewl -> S.EmptyL) = S.fromList $ replicate n x
insertNtime n x ys@(S.viewl -> (y S.:< ys'))
  = case compare x y of
      LT -> y S.<| insertNtime n x ys'
      _ -> foldr (S.<|) ys (replicate n x)

insertBy :: Ord a => (a -> a -> Ordering) -> a -> Seq a -> Seq a
insertBy _   x (S.viewl -> S.EmptyL) = S.singleton x
insertBy cmp x ys@(S.viewl -> (y S.:< ys'))
 = case cmp x y of
     GT -> y S.<| insertBy cmp x ys'
     _  -> x S.<| ys



splitStall::Int -> (Int, Int)
splitStall x = ((x -1 - halfX), halfX)
  where
    halfX = div (x-1) 2
