module Examples where

doubleYou x = x + x
doubleUs x y = x * 2 + y * 2
tribleMe x = 3 * x
ddoubleMe x = doubleYou (doubleYou x)

doubleSmallNumber x = if x > 100
                      then x
                      else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
