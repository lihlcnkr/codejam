module CodeJam where

import Data.List


splitDatas:: [String] -> (String -> Int) -> [[String]]
splitDatas xs f = snd $ foldl foldSplit (0,[]) xs
  where foldSplit (a, b) y = if a > 0
                             then ((a-1),
                                    if length b > 0
                                    then (init b) ++ [(last b) ++ [y]]
                                    else [[y]])
                             else ((f y),b++[[y]])




class CodeJam a where
  codeJamCalc :: [a] -> String
  getTotalLineCount                  :: a -> Int
