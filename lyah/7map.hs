import qualified Data.Map as Map

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' [] = Map.empty
fromList' ((k,v):xs) = Map.insert k v $ fromList' xs

fromList'' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList'' =
  let insertInto = (\acc (k,v) -> Map.insert k v acc)
  in foldl insertInto Map.empty

makeTuples :: [a] -> [b] -> [(a,b)]
makeTuples _ [] = []
makeTuples [] _ = []
makeTuples (x:xs) (y:ys) = [(x,y)] ++ makeTuples xs ys
