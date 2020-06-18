{-
Use haskell construct:
* Integer prefix tree datat struct
* Insert operation in the tree
* Lookup operation in the tree
-}

import Data.Bits

type Key = Int         -- the type of the key
type Prefix = Int      -- the type of longest common prefix of sub childrens
type Mask = Int        -- mask tell where common prefix ends

data IntPrefixTree a = Empty
                     | Leaf Key a
                     | Branch Prefix Mask (IntPrefixTree a) (IntPrefixTree a) deriving Show

{-Some Util Functions-}
mask x m = (x .&. complement (m - 1)) -- mask out the prefix

match k p m = (mask k m) == p         -- mask out and test the prefix

zero x m = x .&. (shiftR m 1) == 0        -- get the bit next to the mask

highestBit x = if x == 0 then 0 else 1 + highestBit (shiftR x 1)

lcp :: Prefix -> Prefix -> (Prefix, Mask) -- generate the largest common prefix and the mask                               
lcp p1 p2 = (p, m) where
  m = bit (highestBit (p1 `xor` p2))
  p = mask p1 m

{- join two(Node or Tree), which p1 and p2 as the prefix -}
join p1 t1 p2 t2 = if (zero p1 m) then Branch p m t1 t2
                                  else Branch p m t2 t1
  where
    (p, m) = lcp p1 p2

{- insert -}
insert t k x
  = case t of
      Empty -> Leaf k x
      Leaf k' x' -> if k == k' then Leaf k x
                    else join k (Leaf k x) k' t -- t@(Leaf k' v')
      Branch p m l r
        | match k p m -> if zero k m
                         then Branch p m (insert l k x) r
                         else Branch p m l (insert r k x)
        | otherwise -> join k (Leaf k x) p t -- t@(Branch p m l r)

genIntPrefixTree kvs = foldl (\acc (k,v) -> insert acc k v) Empty kvs
