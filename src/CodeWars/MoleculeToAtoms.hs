module CodeWars.MoleculeToAtoms where

import Debug.Trace
import System.Random
import Control.Monad.State.Lazy


debug = flip trace

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = Left "Not a valid molecule"

parseM :: String -> String -> Either String [(String,Int)]
parseM = undefined

-- randomSt :: (Random a, RandomGen g) => State g a


