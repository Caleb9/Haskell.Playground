import Data.Char
import Control.Monad

-- main = do
--   contents <- getContents
--   putStr $ shorterThanTen contents

main = interact shorterThanTen

shorterThanTen :: String -> String
shorterThanTen input =
  let allLines = lines input
      shortLines = filter (\line -> length line <= 10) allLines
  in unlines shortLines
