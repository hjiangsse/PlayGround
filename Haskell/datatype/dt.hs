import Data.Map hiding (foldr)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- data Person = Person String String Int Float String String deriving (Show)
-- guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- 
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _ ) = firstname
-- 
-- secondName :: Person -> String
-- secondName (Person _ secondname _ _ _ _ ) = secondname
-- 
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
-- 
-- height :: Person -> Float  
-- height (Person _ _ _ height _ _) = height  
--   
-- phoneNumber :: Person -> String  
-- phoneNumber (Person _ _ _ _ number _) = number  
--   
-- flavor :: Person -> String  
-- flavor (Person _ _ _ _ _ flavor) = flavor

-- data Person = Person { firstName :: String  
                     -- , lastName :: String  
                     -- , age :: Int  
                     -- , height :: Float  
                     -- , phoneNumber :: String  
                     -- , flavor :: String  
                     -- } deriving (Show)

-- data Car a b c = Car { company :: a
                     -- , model :: b
                     -- , year :: c
                     -- } deriving (Show)

data SmallCar = SmallCar { company :: String
                         , model :: String
                         , year :: Int
                         } deriving (Show)

tellSmallCar :: SmallCar -> String
tellSmallCar (SmallCar {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data People = People { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNum = String
type Name = String
type PhoneBook = [(PhoneNum, Name)]

phoneBook :: PhoneBook
phoneBook =    
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]  

inPhoneBook :: Name -> PhoneNum -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type IntMap v = Map Int v
stuLst = fromList([(1, "hjiang"),(2, "nsxia"),(3, "zhang")])

data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleTon :: a -> Tree a
singleTon x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleTon x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno True = True
  yesno False = False

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
