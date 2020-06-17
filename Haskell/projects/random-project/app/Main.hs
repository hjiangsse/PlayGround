module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  (seed:xs) <- getArgs
  genRandomStr (read seed)
