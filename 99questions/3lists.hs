import Data.List
  (nub
  ,subsequences
  ,delete
  ,permutations
  ,sort)
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
  
-- 24

diff_select :: Int -> Int -> IO [Int]
diff_select n range = do
  gen <- newStdGen
  let selectFromSet _ _ 0 = []
      selectFromSet set gen' n' =
        let (index, newGen) = randomR (0, length set - 1) gen'
            x = (set !! index)
        in x : selectFromSet (delete x set) newGen (n'-1)
  return $ (selectFromSet [1..range] gen n)

diff_select' :: Int -> Int -> IO [Int]
diff_select' n m
  | n > m = error "n must be less than or equal to m"
  | otherwise = do
      gen <- newStdGen
      return (take n . nub . randomRs (1, m) $ gen)

-- 25

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  gen <- newStdGen
  let rnd_permu' _ [] = []
      rnd_permu' gen' xs' =
        let (index, newGen) = randomR (0, length xs' - 1) gen'
        in  xs' !! index : rnd_permu' newGen (take index xs' ++ drop (index+1) xs')
  return (rnd_permu' gen xs)

-- 26

-- Very inefficient for larger lists
combinations :: (Ord a) => Int -> [a] -> [[a]]
combinations k xs =
  let dupli_combinations = map (sort . take k) . permutations
  in sort . nub . dupli_combinations $ xs
