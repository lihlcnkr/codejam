module FasionShow where


import Data.Char
import Data.Sequence(Seq)
import qualified Data.Sequence as S

import Control.Monad.ST
import Data.STRef
import Control.Monad

import System.Random

import           Data.Vector.Unboxed         (freeze, toList)
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.State
import           Data.Vector.Unboxed ((!), (//))
import           Control.Monad.Primitive     (PrimMonad, PrimState)

import qualified Data.Vector.Algorithms.Merge as M

getBathroomStalls :: String -> String
getBathroomStalls xs =
  let inputs = words xs
      ys = [read (head inputs)::Int]
      y = read (last inputs)::Int
  in show 1 ++ " " ++ (show 2)


fibST :: Int -> Int
fibST n
  | n < 2 = n
  | otherwise = runST (test2 n)


test :: Int -> ST s (V.MVector s Int)
test n = do
  v <- V.new (n+1)
  return v

test2 :: Int -> ST s Int
test2 n = do
  v <- test n           -- call test, which performs the allocation
  fibST' n v


fibST' :: Int -> (V.MVector s Int) -> ST s Int
fibST' n v
  | n < 2 = do
      V.write v n n
      return n
  | otherwise = do
      fibST' (n-1) v
      nMinus1 <- V.read v (n-1)
      nMinus2 <- V.read v (n-2)
      V.write v n (nMinus2 + nMinus1)
      return (nMinus2 + nMinus1)

printRandomArry s n = runST (printRandomArry' s n)

printRandomArry' :: StdGen -> Int -> ST s [Int]
printRandomArry' g n = do
  v <- test n
  printRun g v 0 n
  M.sort v
  v' <- freeze v
  return (toList v')


printRun :: StdGen -> (V.MVector s Int) -> Int -> Int -> ST s ()
printRun g v x y
  | x < y = do
      let (r,g') = randomR (1,100) g
      V.write v x r
      printRun g' v (x+1) y
      return ()
  | otherwise = do
      return ()

