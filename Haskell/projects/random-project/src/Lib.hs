module Lib
    ( someFunc,
      randomTest,
      genRandomStr
    ) where

import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randomTest :: Int -> IO ()
randomTest seed = do
  let (first, second, third) = threeCoins (mkStdGen seed)
  putStrLn (show first)
  putStrLn (show second)
  putStrLn (show third)

genRandomStr :: Int -> IO ()
genRandomStr len = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen)
