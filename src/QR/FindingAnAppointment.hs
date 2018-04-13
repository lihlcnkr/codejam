module QR.FindingAnAppointment where

getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules duration = undefined


convertToIntTime :: String -> Int
convertToIntTime x = 
  let (ys, (z:zs)) = span (/= ':') x
      hour = read ys::Int
      min = read zs::Int
  in hour * 60 + min

convertTime :: (String, String) -> (Int, Int)
convertTime (x,y) = (convertToIntTime x, convertToIntTime y)

--getFirstTimeDuration:: [[(Int,Int)]] -> 
