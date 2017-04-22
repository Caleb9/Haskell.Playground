doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
  then x
  else x * 2

-- Explicit type
removeNonUpperCase :: String -> String
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- Recursive factorial
factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' x = x * factorial' (x - 1)

factorial'' :: Integer -> Integer
factorial'' x
  | (x == 0) = 1
  | otherwise = x * factorial'' (x - 1)

max' :: (Ord a) => a -> a -> a
max' x y
  | x > y = x
  | otherwise = y

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're skinny!"
  | otherwise = "You're not skinny."
  where bmi = weight / height ^ 2
        skinny = 18.5

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let circumference = 2 * pi * r
      sideArea = circumference * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

testCase :: Integer -> String
testCase x = case x of 10 -> "It is ten!"
                       otherwise -> "It is not ten."
