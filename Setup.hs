import Distribution.Simple
import Data.Set

type SleepCount = Either Int String


checkNumberContains:: String -> Set Int -> Set Int
checkNumberContains xs ss
  | xs > 9 =
      let m = div xs 10
          n = (xs - 10 * m)
        in checkNumberContains m (insert n ss)
  | otherwise = xs

main = do
  let ss = checkNumberContains 7341 empty
  putStrLn $ show ss
