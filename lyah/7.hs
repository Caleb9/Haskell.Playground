import Data.List (nub)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' x [y] = [y]
intersperse' x (y:ys) = y : x : intersperse' x ys

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k,v):xs) =
  if key == k
  then Just v
  else findKey' key xs

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key =
  foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z
