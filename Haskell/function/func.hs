sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

prec :: (Integral a) => a -> a
prec x = (x - 1)

myFact :: (Integral a) => a -> a
myFact 0 = 1
myFact x = x * myFact (prec x)

addVector :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

triFirst :: (a, b, c) -> a
triFirst (x, _, _) = x

triSecond :: (a, b, c) -> b
triSecond (_, y, _) = y

triThird :: (a, b, c) -> c
triThird (_, _, z) = z

doubleLst :: (Num a) => [a] -> [a]
doubleLst lst = [2 * x | x <- lst]

mylstLen :: (Num a) => [b] -> a
mylstLen [] = 0
mylstLen (_:xs) = 1 + (mylstLen xs)

tellLst :: (Show a) => [a] -> String
tellLst [] = "The list is empty"
tellLst (x:[]) = "The list has one element: " ++ show x
tellLst (x:y:[]) = "The list has two element: " ++ show x ++ " and " ++ show y
tellLst (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

tellFirstLetter :: String -> String
tellFirstLetter "" = "Empty string, whoops!"
tellFirstLetter all@(x:xs) = "The first letter of " ++ (x:xs) ++ " is " ++ [x]

callBMI :: (RealFloat a) => a -> a -> a
callBMI weight height = weight / (height * height)

littleDoctor :: (RealFloat a) => a -> a -> String
littleDoctor weight height
  | bmi <= skinny = "you are under weight!"
  | bmi <= normal = "Normal a!"
  | bmi <= fat = "fat! a!"
  | otherwise = "BOOM!"
  where bmi = (callBMI weight height)
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

newInitials :: String -> String -> String
newInitials (a:_) (b:_) = [a] ++ " . " ++ [b] ++ "."

calBims :: (RealFloat a) => [(a, a)] -> [a]
calBims xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / (height ^ 2)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in sideArea + 2 * topArea

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty list!"
                      (x:_ ) -> x
