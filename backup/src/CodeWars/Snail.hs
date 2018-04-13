module CodeWars.Snail where


snail :: [[Int]] -> [Int]
snail xs = snailOnce xs TOP []

data SnailType = TOP | RIGHT | BOTTOM | LEFT

snailOnce :: [[Int]] -> SnailType -> [Int] -> [Int]
snailOnce [[]] _ zs = zs
snailOnce [] _ zs = zs
snailOnce (x:xs) TOP zs = snailOnce xs RIGHT (zs ++ x) 
snailOnce xs RIGHT zs =
  let xs' = fmap (\c -> splitAt ((length c) -1) c) xs
    in snailOnce (fmap (\(a,_) -> a) xs') BOTTOM (zs ++ (fmap (\(_,b) -> (head b)) xs')) 
snailOnce xs BOTTOM zs =
  let (xs', x') = splitAt ((length xs) -1) xs
  in snailOnce xs' LEFT (zs ++ (reverse (head x')))

snailOnce xs LEFT zs = snailOnce (fmap tail xs) TOP (zs ++ (reverse (fmap head xs)))
