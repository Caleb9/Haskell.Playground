import Data.List (nub,subsequences)
import System.Random

-- 20

removeAt :: Int -> [a] -> (a, [a])
removeAt l xs = (xs !! k, take k xs ++ drop l xs)
  where k = l-1

-- 21

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = first ++ x:last
  where (first, last) = splitAt (k-1) xs
  
-- 22

range :: Int -> Int -> [Int]
range a b = [a..b]

-- 23

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- newStdGen
  return $ rnd_select' xs gen n
  where rnd_select' :: [a] -> StdGen -> Int -> [a]
        rnd_select' _ _ 0 = []
        rnd_select' xs' gen' n' =
          let (index, newGen) = randomR (0, length xs' - 1) gen'
          in (xs' !! index) : rnd_select' xs' newGen (n'-1)

rnd_select'' :: [a] -> Int -> IO [a]
rnd_select'' xs n = do
  gen <- newStdGen
  return $ take n [xs !! x | x <- randomRs (0, (length xs)-1) gen]
  
