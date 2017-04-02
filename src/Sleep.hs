module Sleep where

import Data.Set
import Control.Monad.State.Lazy

type SleepCount = Either Int String

--data SleepCount = SleepCount (startNum::Int, nums::Set Int, current::Int) deriving Show

type SleepStateMonad  = State SleepCount 

checkNumberContains:: Int -> Set Int -> Set Int
checkNumberContains x ss
  | x > 9 =
      let m = div x 10
          n = (x - 10 * m)
        in checkNumberContains m (insert n ss)
  | otherwise = insert x ss


getSleepNumber:: Int -> (Set Int) -> Int -> Int -> SleepCount
getSleepNumber 0 _ _ _= Right "INSOMNIA"
getSleepNumber base xs current index = 
  let nextNums = checkNumberContains current xs
      nextSleep = base * (index + 1)
   in  if size nextNums >= 10 then Left current else getSleepNumber base nextNums nextSleep (index+1)

-- getSleepNumber:: SleepStateMonad -> Either Int String 
-- getSleepNumber = do
--   s <- get
  
--   let sleepNums = nums s
--   let 




