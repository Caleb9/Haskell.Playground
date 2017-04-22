import qualified Data.List (group)

-- 11
data ListItem a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs =
  [y | x <- Data.List.group xs, let y = if length x > 1
                                        then Multiple (length x) (head x)
                                        else Single (head x)]

-- 12
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified = concat . foldr (\x acc -> (fromListItem x) : acc) []
  where fromListItem (Multiple n a) = replicate n a
        fromListItem (Single a) = [a]

decodeModified' :: (Eq a) => [ListItem a] -> [a]
decodeModified' = 
  let fromListItem (Multiple n a) = replicate n a
      fromListItem (Single a) = [a]
  in concat . foldr (\x acc -> (fromListItem x) : acc) []

decodeModified'' :: (Eq a) => [ListItem a] -> [a]
decodeModified'' = concatMap fromListItem
  where fromListItem (Multiple n x) = replicate n x
        fromListItem (Single x) = [x]

-- {from P10}
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- Data.List.group xs]

decode :: Eq a => [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)

decode' :: Eq a => [(Int, a)] -> [a]
decode' = foldr f []
  where f (1, x) acc = x : acc
        f (k, x) acc = x : f (k-1, x) acc

toTuple :: ListItem a -> (Int, a)
toTuple (Single x) = (1, x)
toTuple (Multiple n x) = (n, x)

decodeModified''' :: (Eq a) => [ListItem a] -> [a]
decodeModified''' = concatMap (uncurry replicate . toTuple)

-- 13 TODO

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' xs = concat [[x,x] | x <- xs]

dupli'' :: [a] -> [a]
dupli'' = concatMap (\x -> [x,x])

dupli''' :: [a] -> [a]
dupli''' = concatMap (replicate 2)

dupli'''' :: [a] -> [a]
dupli'''' = foldl (\acc x -> acc ++ [x,x]) []

dupli''''' :: [a] -> [a]
dupli''''' = foldr (\x acc -> x:x:acc) []

-- 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n
  | n < 1 = error "n must be a positive integer"
  | length xs < n = xs
  | otherwise = take (n-1) xs ++ dropEvery (drop n xs) n

-- 17 TODO

-- this one uses "predefined predicates"
split :: [a] -> Int -> ([a], [a])
split xs n
  = (take n xs, drop n xs)

split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' list count = (take' count list, drop' count list)
  where
    take' n all@(x:xs)
      | n == 0 = []
      | n >= length all = all
      | otherwise = x : take' (n-1) xs
    drop' n all@(x:xs)
      | n == 0 = all
      | n >= length all = []
      | otherwise = drop' (n-1) xs

split'' :: [a] -> Int -> ([a], [a])
split'' [] _ = ([], [])
split'' l@(x : xs) n | n > 0 = (x : ys, zs)
                     | otherwise = ([], l)
  where (ys, zs) = split'' xs (n-1)

-- 18

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i k
  | i < 1 || i > length xs || k < i || k > length xs = error "Invalid range"
  | otherwise = take (k-i+1) . drop (i-1) $ xs

-- 19

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs i = drop count xs ++ take count xs
  where count = abs(i `mod` (length xs))
