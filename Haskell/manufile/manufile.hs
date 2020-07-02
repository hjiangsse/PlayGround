import System.IO
import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are: "
  putStrLn $ unlines (zipWith (\n arg -> show n ++ " : " ++ arg) [1..] args)
  putStrLn "The name of the program is: "
  putStrLn progName
  

{-
main = do
  contents <- readFile "test.txt"
writeFile "testcaps.txt" (map toUpper contents)
-}

{-
main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
-}

{-
main = do
  withFile "test.txt" ReadMode (\handle -> do
      hSetBuffering handle $ BlockBuffering (Just 2048)
      contents <- hGetContents handle
      putStr contents)
-}

{-
main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TO-DO items: "
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
-}

