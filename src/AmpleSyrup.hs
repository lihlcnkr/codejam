module AmpleSyrup where

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Prime
import Data.Maybe


--pi :: Double
--pi = 3.14159265359


getCoinJam :: (String, [String]) -> String
getCoinJam (x,ys) = show $ getAmpleSyrup z ys'
  where datas = words x
        y = read (head datas)::Int
        z = read (last datas)::Int
        ys' = fmap getDoubleDat ys

getDoubleDat :: String -> (Double, Double)
getDoubleDat x = (y, z)
  where datas = words x
        y = read (head datas)::Double
        z = read (last datas)::Double

getAmpleSyrup :: Int -> [(Double,Double)] -> Double
getAmpleSyrup y zs  =
  let zs' = sortBy sortByDesc zs
  in getAmpleSyrup' y zs'

getAmpleSyrup' :: Int -> [(Double,Double)] -> Double
getAmpleSyrup' y zss@(z:zs)
  | y >= (length zss) = getCirleSuface (fst z) + sum (fmap getHeightSuface zss)
  | otherwise =
      let
        fcs = getCirleSuface (fst z)
        sh = head zs
        scs = getCirleSuface (fst sh)
        fs = fcs + getHeightSuface z
        ss = scs + getHeightSuface (zs !! (y-1))
      in if fs > ss then fcs + sum (take y (sortBy (\x' y' -> compare y' x') (fmap getHeightSuface zss))) else getAmpleSyrup' y zs


--take 3 $ snd $ mapAccumL isCoinJam s $ getPosibleList 6

getCirleSuface :: Double -> Double
getCirleSuface x = x * x * pi

getHeightSuface :: (Double, Double) -> Double
getHeightSuface (x,y) = 2 * x * y * pi

sortByDesc :: (Double, Double) -> (Double, Double) -> Ordering
sortByDesc xs ys = compare (fst ys) (fst xs)








