import Data.List
       ( delete )
import Data.Maybe
       ( fromMaybe )
import System.Directory
       ( removeFile
       , renameFile )
import System.Environment
       ( getArgs )
import System.IO
       ( appendFile
       , readFile
       , openTempFile
       , putStr
       , hPutStr
       , hClose )

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

main = do
  (command:args) <- getArgs
  let action = fromMaybe (\_ -> errorExit) . lookup command $ dispatch
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = errorExit

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks
view _ = errorExit

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      newTodoTasks = delete (todoTasks !! read numberString) todoTasks
  (tempPath, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines newTodoTasks
  hClose tempHandle
  removeFile fileName
  renameFile tempPath fileName
remove _ = errorExit

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let number = read numberString
      todoTasks = lines contents
      bumpedTask = todoTasks !! number
      newTodoTasks = bumpedTask : (delete (todoTasks !! number) todoTasks)
  (tempPath, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines newTodoTasks
  hClose tempHandle
  removeFile fileName
  renameFile tempPath fileName
bump _ = errorExit

errorExit :: IO ()
errorExit = error "Invalid input"
