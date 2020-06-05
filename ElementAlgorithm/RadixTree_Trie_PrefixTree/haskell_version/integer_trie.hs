import Data.List

data IntTrie a = Empty | Branch (IntTrie a) (Maybe a) (IntTrie a) deriving Show

-- make a singleton tree(only one node)
singletonTrie :: a -> IntTrie a
singletonTrie x = Branch Empty (Just x) Empty

-- get the left tree
left :: IntTrie a -> IntTrie a
left (Branch l _ _ ) = l
left Empty = Empty

-- get the right tree
right :: IntTrie a -> IntTrie a
right (Branch _ _ r) = r
right Empty = Empty

-- get the value of a tree node
value :: IntTrie a -> Maybe a
value (Branch _ v _) = v
value Empty = Nothing

-- insert a (key, value) into the trie
insertTrie :: Integral a1 => IntTrie a2 -> a1 -> a2 -> IntTrie a2
insertTrie t 0 x = Branch (left t) (Just x) (right t)
insertTrie t k x
  | even k = Branch (insertTrie (left t) (k `div` 2) x) (value t) (right t)
  | otherwise = Branch (left t) (value t) (insertTrie (right t) (k `div` 2) x)

-- insert pair (k,v) into a trie
inspair :: Integral a1 => IntTrie a2 -> (a1, a2) -> IntTrie a2
inspair t (k, v) = insertTrie t k v

-- search a trie using a key
searchTrie Empty k = Nothing -- search an empty trie, find nothing
searchTrie t 0 = value t -- give current root value when search 0
searchTrie t k = if even k then searchTrie (left t) (k `div` 2)
                 else searchTrie (right t) (k `div` 2)
