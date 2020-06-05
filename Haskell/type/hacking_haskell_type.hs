import Data.Char

newadd left right = (ord left) + (ord right)

getlen [] = 0
getlen (x:xs) = 1 + getlen xs

data TrafficLight = Red | Yellow | Green

data BiTree a = Empty | BiNode a (BiTree a) (BiTree a)

qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x]
               ++ [x]
               ++ qsort [y | y <- xs, y >= x]
