-- https://wiki.haskell.org/99_questions/

import qualified Data.List (group, findIndex)

-- 1
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast x = head (reverse x)

-- 2
myButLast :: [a] -> a
myButLast [] = myLast []
myButLast [x] = error "Minimum of two elements lists!"
myButLast x = x !! ((length x) - 2)

myButLast' :: [a] -> a
myButLast' [] = myLast []
myButLast' [x] = error "Minimum of two elements lists!"
myButLast' x = reverse x !! 1

-- 3
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl (\acc _ -> acc + 1) 0

myLength'' :: [a] -> Int
myLength'' = foldr (\_ acc -> acc + 1) 0

myLength''' :: [a] -> Int
myLength''' xs = acc xs 0
  where acc [] n = n
        acc (_:xs) n = acc xs $ n + 1

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> flip (:) acc x) []

myReverse'' :: [a] -> [a]
myReverse'' xs = foldr (\x fId empty -> fId (x : empty)) id xs []

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome list@(x:xs)
  = let y = last list
    in if (x == y)
       then isPalindrome . tail . init $ list
       else False
  
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == reverse xs

---- less efficient with fold
isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs
  = foldl (\acc (x,y) -> acc && x == y) True input
  where input = zip xs $ reverse xs

isPalindrome''' xs
  = foldl (\acc (x,y) -> if x == y then acc else False) True input
  where input = zip xs $ reverse xs

-- 7
data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq)

flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List []) = []
flatten' (List (x:xs))
  = foldr (:) (flatten' (List xs)) (flatten' x)

flatten'' :: NestedList a -> [a]
flatten'' (Elem a) = [a]
flatten'' (List xs)
  = foldr (++) [] $ map flatten'' xs

flatten''' :: NestedList a -> [a]
flatten''' (Elem x) = [x]
flatten''' (List a) = concatMap flatten''' a

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
  | xs == [] = [x]
  | x == head xs = compress xs
  | otherwise = x : compress xs

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs)
  = foldl (\acc e -> if e == (last acc) then acc else acc ++ [e]) [x] xs

compress'' :: (Eq a) => [a] -> [a]
compress'' = map head . Data.List.group

-- 9
pack :: Eq a => [a] -> [[a]]
pack = Data.List.group

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' all@(x:xs)
  = takeWhile (== x) all : (pack' $ dropWhile (==x) all)

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs)
  = let (first, rest) = span (==x) xs
    in (x:first) : pack'' rest

pack''' :: Eq a => [a] -> [[a]]
pack''' [] = []
pack''' (x:xs) = (x:reps) : (pack''' rest)
  where
    findIndex = Data.List.findIndex
    (reps, rest)
          = maybe (xs,[]) (\i -> splitAt i xs) (findIndex (/=x) xs)

pack'''' :: Eq a => [a] -> [[a]]
pack'''' = foldr func []
  where func x [] = [[x]]
        func x (y:ys) = if (x == head y) then (x:y):ys else [x]:y:ys

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs
  = let group = Data.List.group xs
    in foldr (\list acc -> (length list, head list):acc) [] group

encode' :: Eq a => [a] -> [(Int, a)]
encode' xs = [(length x, head x) | x <- Data.List.group xs]

encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = map (\x -> (length x, head x)) . Data.List.group

encode''' :: Eq a => [a] -> [(Int, a)]
encode''' [] = []
encode''' (x:xs) =
  (length $ x:takeWhile (==x) xs, x) : (encode''' . dropWhile (==x) $ xs)
