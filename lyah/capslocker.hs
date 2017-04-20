import Data.Char
import Control.Monad

-- main = forever $ do
--   putStrLn "Give me some input:"
--   l <- getLine
--   putStrLn $ map toUpper l

main = do
  contents <- getContents
  putStr $ map toUpper contents
