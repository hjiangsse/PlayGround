-- main = putStrLn "Hello World"

--main = do
--  putStrLn "Hello, What is your name?"
--  name <- getLine
--  putStrLn ("Hey " ++ name ++ ", you rock!")
--  
--  putStrLn "Hello, How old are you?"
--  age <- getLine
--  putStrLn ("Hey " ++ age ++ " Now!!")

--tellFuture:: String -> String
--tellFuture name = "This is your future " ++ name
--
--main = do
--  foo <- putStrLn "Tell me your name: "
--  name <- getLine
--  putStrLn ("Future: " ++ tellFuture(name))

{-
import Data.Char

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
-}

{-
main = do
  line <- getLine
  if null line
    then return ()
    else do
       putStrLn $ reverseWords line
       main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-}

{-
main = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line
-}

{-
main = do
  a <- return "hell"
  b <- return "year"
  putStrLn $ a ++ " " ++ b
-}

{-
main = do putStr "Hey, "
          putStr "I'm "
          putStr "Hany!"


myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
  putChar x
  myPutStr xs
-}

{-
main = do putChar 'T'
          putChar 'E'
          putChar 'D'
          myPutStr "\nWelcome to TED!!!\n"


main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3] 

main = do
  c <- getChar
  if c /= ' '
    then do
        putChar c
        main
    else
        return ()
-}

--import Control.Monad

{-
main = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main

main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs
-}

import Control.Monad
import Data.Char

main = forever $ do
  putStr "Give me some food: "
  input <- getLine
  putStrLn $ map toUpper input
