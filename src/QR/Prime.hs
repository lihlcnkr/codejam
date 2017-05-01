module Prime where

import Data.List

import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe

import Control.Monad.State.Lazy
import System.Random


data PrimeRepo = PrimeRepo {primeSet::Set Int, maxChecked::Int} deriving Show

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral


testState :: Int -> [String]
testState = do
  evalState $ do
    b <- state convertToStr
    c <- state convertToStr
    d <- state convertToStr
    return $ [b,c,d]


convertToStr :: Int -> (String, Int)
convertToStr a = (show a, a+1)


makeRandom :: (RandomGen g) => Int -> g -> (Int, g)
makeRandom a = randomR (10,a)


doRandom :: (RandomGen g) => g -> [Int]
doRandom = evalState $ do
  b <- state $ makeRandom 20
  c <- state $ makeRandom 100
  d <- state $ makeRandom 1000
  return $ [b,c,d]



runWithScope :: Int -> PrimeRepo -> (Maybe Int, PrimeRepo)
runWithScope a s@(PrimeRepo {primeSet=ps, maxChecked=b})
  | a == b+1 = divisorOutScope a s
  | a > b =
    let (c, ds) = runWithScope (b+1) s
    in runWithScope a ds
  | otherwise = divisorInScope a s


divisorOutScope :: Int -> PrimeRepo -> (Maybe Int, PrimeRepo)
divisorOutScope a = evalState $ do
  b <- state $ divisorInScope a
  s@(PrimeRepo {primeSet=sSet, maxChecked=_}) <- get
  case b of Nothing -> do
              let m = S.insert a sSet
              return (b, PrimeRepo {primeSet=m,maxChecked=a})
            _ -> do
              return (b, PrimeRepo {primeSet=sSet,maxChecked=a})

divisorInScope :: Int -> PrimeRepo -> (Maybe Int, PrimeRepo)
divisorInScope a r = (b, r)
  where
    b = S.foldl (\y x -> if isJust y then y else if mod a x == 0 then Just x else Nothing) Nothing (primeSet r)

-- getDivisors :: Int -> PrimeRepo -> (Maybe Int, PrimeRepo)
-- getDivisors 1 s = (Nothing, s)
-- getDivisors a s
--   | a > maxChecked s = 
--   | otherwise = case lookup


--getDivisor ::

-- getDevisorAndSet :: Int -> PrimeRepo -> (Maybe Int, PrimeRepo)
-- getDevisorAndSet a s
--   | a > maxChecked s = evalState $ do
      
--   | member a (primeSet s) = (Nothing, s)
--   | otherwise = (b, ss)
--     where (b, ss) = getDivisor a (primeSet s)



-- getDivisorInScope :: Int -> PrimeRepo -> (Maybe Int, PrimeRepo)
-- getDivisorInscope a r = case b of Nothing -> (Nothing, PrimeRepo (S.insert a r))
--                            Just c -> (b, s)
--   where
--     ss 
--     b = S.foldl (\y x -> if isJust y then y else if mod a x == 0 then Just x else Nothing) Nothing s
