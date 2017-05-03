module AmpleSyrup where

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Prime
import Data.Maybe

import Text.Printf
import CodeJam
--pi :: Double
--pi = 3.14159265359



newtype AmpleSyrup = AmpleSyrup [String]



getCoinJam :: [String] -> String
getCoinJam (x:ys) = printf "%.9f" $ getAmpleSyrup z ys'
  where datas = words x
        y = read (head datas)::Int
        z = read (last datas)::Int
        ys' = fmap getDoubleDat ys


instance CodeJam AmpleSyrup where
  codeJamCalc (AmpleSyrup (x:ys)) = printf "%.9f" $ getAmpleSyrup z ys'
    where datas = words x
          y = read (head datas)::Int
          z = read (last datas)::Int
          ys' = fmap getDoubleDat ys

  getData (AmpleSyrup xs) = xs

  toCodeJam xs = AmpleSyrup xs




getTotalLineCount :: String -> Int
getTotalLineCount x =
    let datas = words x
    in read (head datas)



getDoubleDat :: String -> (Double, Double)
getDoubleDat x = (y, z)
  where datas = words x
        y = read (head datas)::Double
        z = read (last datas)::Double

getAmpleSyrup :: Int -> [(Double,Double)] -> Double
getAmpleSyrup y zs  =
  let zs' = sortBy sortByDesc zs
  in syrupCall y zs' 0 0


syrupCall :: Int -> [(Double, Double)] -> Double -> Double -> Double
syrupCall x yss@(y:ys) a b
  | x >= length yss = max b (getAmpleSyrup' x yss)
  | otherwise =
    let fcs = getCirleSuface (fst y) + (getHeightSuface y)
    in if fcs > a
       then
         let fcs' = getAmpleSyrup' x yss
         in if fcs' > b then syrupCall x ys fcs fcs' else syrupCall x ys a b
       else syrupCall x ys a b

--getAmpleSyrup' :: Int -> [(Double,Double)] -> Double
--getAmpleSyrup' y zss@(z:zs) = getCirleSuface (fst z) + sum (fmap getHeightSuface zss)

getAmpleSyrup' :: Int -> [(Double,Double)] -> Double
getAmpleSyrup' y zss@(z:zs)
  | y >= (length zss) = getCirleSuface (fst z) + sum (fmap getHeightSuface zss)
  | otherwise = 
      let
        fcs = getCirleSuface (fst z)
      in fcs + (getHeightSuface z) + sum (take (y-1) (sortBy (\x' y' -> compare y' x') (fmap getHeightSuface zs)))





--take 3 $ snd $ mapAccumL isCoinJam s $ getPosibleList 6

getCirleSuface :: Double -> Double
getCirleSuface x = x * x * pi

getHeightSuface :: (Double, Double) -> Double
getHeightSuface (x,y) = 2 * x * y * pi

sortByDesc :: (Double, Double) -> (Double, Double) -> Ordering
sortByDesc xs ys = compare (fst ys) (fst xs)








