import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x == True = True
  | otherwise = myAny f xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs)
  | f x == False = False
  | otherwise = myAll f xs

quickDivide :: (a->Bool) -> [a] -> ([a], [a])
-- we use partition instead of span or break, because we must pass the whole list
quickDivide f lst = partition f lst


encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted

decode :: Int -> String -> String
decode lshift msg =
  let ords = map ord msg
      shifted = map (\x -> x - lshift) ords
  in map chr shifted

