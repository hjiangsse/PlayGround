import Data.Char

{-
main = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn $ map toUpper l
-}

{-
main = do
  contents <- getContents
  putStr (shortLinesOnly contents)
-}

main = interact reverseStr

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

reverseStr :: String -> String
reverseStr str = reverse str
