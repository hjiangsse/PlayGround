lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "This the first class of haskell"
sayMe 2 = "Next day is another day"
sayMe 3 = "Three man is better than one man"
sayMe 4 = "four blooding heart"
sayMe 5 = "five states come together"
sayMe x = "no no no"

-- make our own typeclass and make type instance
-- typeclass in haskell is something like a protocol
-- if your type match all standards in the protocol
-- then we say your type is an instance of the typeclass
class NewEq equatable where
  neweq :: equatable -> equatable -> Bool
  newne :: equatable -> equatable -> Bool
  x `neweq` y = not (x `newne` y)
  x `newne` y = not (x `neweq` y)

class NewShow showable where
  newshow :: showable -> String

data TrafficLight = Red | Yellow | Green

instance NewEq TrafficLight where
-- because the class alreay know the relationship of its function
-- accoring the class definition
  Red `neweq` Red = True
  Green `neweq` Green = True
  Yellow `neweq` Yellow = True
  _ `neweq` _ = False

instance NewShow TrafficLight where
  newshow Red = "[Red Light]"
  newshow Green = "[Green Light]"
  newshow Yellow = "[Yellow Ligth]"

class Running a where
  running :: a -> String
  jump :: a -> String

data Person = Person { name :: String
                     , talk :: String
                     }

instance Running Person where
  -- jump p = (name p) ++ " is " ++ (show (age p)) ++ " years old, jump"
  running p = (name p) ++ " is running " ++ (talk p)
  jump p  = (name p) ++ " is jumping " ++ (talk p)

