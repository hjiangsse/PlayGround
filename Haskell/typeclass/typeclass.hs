{-# LANGUAGE FlexibleInstances #-}
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

stringEq :: [Char] -> [Char] -> Bool
-- Match if both are empty
stringEq [] [] = True
-- If both start with the same char, check the rest
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
-- Everything else doesn't match
stringEq _ _ = False

class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Color where
    isEqual Red  Red  = True
    isEqual Green Green = True
    isEqual Blue Blue = True
    isEqual _     _     = False

instance BasicEq [Char] where
  isEqual [] [] = True
  isEqual (x:xs) (y:ys) = x == y && isEqual xs ys
  isEqual _ _ = False

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isEqual2 x y = not (isNotEqual2 x y)
  
  isNotEqual2 :: a -> a -> Bool
  isNotEqual2 x y = not (isEqual2 x y)

instance BasicEq2 [Char] where 
  isEqual2 [] [] = True
  isEqual2 (x:xs) (y:ys) = x == y && isEqual2 xs ys
  isEqual2 _ _ = False

instance Show Color where
  show Red = "[RED]"
  show Green = "[GREEN]"
  show Blue = "[BLUE]"
