{-# LANGUAGE FlexibleContexts #-}
maximum' :: (Ord a) => [a] -> a
maximum' [] =  error "can not get maximum from empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

maximumNew :: (Ord a) => [a] -> a
maximumNew [] = error "can not get maximum from empty list"
maximumNew [x] = x
maximumNew (x:xs) = max x (maximumNew xs)

myreplicate :: (Num a, Ord a) => a -> b -> [b]
myreplicate n x
  | n <= 0 = []
  | otherwise = x:(myreplicate (n - 1) x)

mytake :: (Num i, Ord i) => i -> [a] -> [a]
mytake n _
  | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x: mytake (n - 1) xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

myrepeat :: a -> [a]
myrepeat x = x:myrepeat x

myzip :: [a] -> [b] -> [(a, b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y):zip xs ys

myelem :: (Eq a) => a -> [a] -> Bool
myelem _ [] = False
myelem e (x:xs)
  | e == x = True
  | otherwise = myelem e xs

haskquick :: (Ord a) => [a] -> [a]
haskquick [] = [] 
haskquick (pv:xs) =
  let smaller = haskquick [a | a <- xs, a <= pv]
      bigger = haskquick [a | a <- xs, a > pv]
  in  smaller ++ [pv] ++ bigger

-- haskell folds
foldlsum :: (Num a) => [a] -> a
foldlsum xs = foldl (\a b -> a + b) 0 xs

foldlsum' :: (Num a) => [a] -> a
foldlsum' = foldl (+) 0

-- my foldl
myfoldl :: (a -> a -> a) -> a -> [a] -> a
myfoldl f acc xs
  | null xs = acc
  | otherwise = myfoldl f (f acc h) t
  where h = head xs
        t = tail xs

myelem' :: (Eq a) => a -> [a] -> Bool
myelem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- test foldr
mymap :: (a -> b) -> [a] -> [b]
mymap f xs = foldr (\x acc -> f x : acc) [] xs


