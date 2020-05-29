multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

devideByTen :: (Floating a) => a -> a
devideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- apply a function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- use higher order function abstract the common patterns
newZipWith f a b
  | null a = []
  | null b = []
  | otherwise = f ha hb:newZipWith f ta tb
  where ha = (head a)
        hb = (head b)
        ta = (tail a)
        tb = (tail b)

-- myflip: flip the two parameters of the function
myflip :: (a->b->c) -> (b->a->c)
myflip f = g
  where g x y = f y x

-- mymap: apply a function to every element in the list, producing a new list
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x:mymap f xs

dbl x = 2 * x

-- myfilter: take a predicate and a list, then return the list of elems statisfy
-- the predicate
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = [] -- this is the base case
myfilter f (x:xs)
  | f x == True = x: myfilter f xs
  | otherwise = myfilter f xs

isPalindrome :: [Char] -> Bool
isPalindrome word
  | word == revword = True
  | otherwise = False
  where revword = reverse word

-- quicksort use filter
myquicksort ::(Ord a) => [a] -> [a]
myquicksort [] = []
myquicksort (x:xs) = 
  let smallerpart = myquicksort (filter (<=x) xs)
      biggerpart = myquicksort (filter (>x) xs)
  in  smallerpart ++ [x] ++ biggerpart

largestDivisible :: (Integral a) => a
largestDivisible
  = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n * 3 + 1)
