module SparseBinaryRandomAccessList where

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

newtype BinaryRandomAccessList a = BinaryRandomAccessList [Tree a] deriving Show

empty :: BinaryRandomAccessList a
empty = BinaryRandomAccessList []

rank :: Tree a -> Int
rank (Leaf _)     = 0
rank (Node r _ _) = r

link :: Tree a -> Tree a -> Tree a
link left right = Node (rank left + 1) left right

consTree :: Tree a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
consTree tree (BinaryRandomAccessList []) = BinaryRandomAccessList [tree]
consTree tree (BinaryRandomAccessList trees@(first : rest))
    | rank tree < rank first  = BinaryRandomAccessList $ [tree] ++ trees
    | rank tree == rank first = consTree (link tree first) (BinaryRandomAccessList rest)
    | otherwise               = error "rank of tree is greater than rank of first"

cons :: a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
cons = consTree . Leaf

uncons :: BinaryRandomAccessList a -> (Tree a, BinaryRandomAccessList a)
uncons (BinaryRandomAccessList [])                           = error "argument list to uncons is empty"
uncons (BinaryRandomAccessList (first@(Leaf _) : rest))      = (first, BinaryRandomAccessList rest)
uncons (BinaryRandomAccessList ((Node _ left right) : rest)) = (firstInLeft, newList)
    where (firstInLeft, (BinaryRandomAccessList remainingInLeft)) = uncons $ BinaryRandomAccessList [left]
          newList = BinaryRandomAccessList $ remainingInLeft ++ [right] ++ rest

-- | Returns the first element in a list
--
-- >>> SparseBinaryRandomAccessList.head (cons 1 empty)
-- 1
--
-- >>> SparseBinaryRandomAccessList.head (cons 1 (cons 2 empty))
-- 1
--
-- >>> SparseBinaryRandomAccessList.head (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty))))))
-- 1
head :: BinaryRandomAccessList a -> a
head list = case fst $ uncons list of
    (Leaf value) -> value
    _            -> error "uncons doesn't return the head of list"

-- | Returns the tail of a list
--
-- >>> SparseBinaryRandomAccessList.tail (cons 1 empty)
-- BinaryRandomAccessList []
--
-- >>> SparseBinaryRandomAccessList.tail (cons 1 (cons 2 empty))
-- BinaryRandomAccessList [Leaf 2]
--
-- >>> SparseBinaryRandomAccessList.tail (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty))))))
-- BinaryRandomAccessList [Leaf 2,Node 2 (Node 1 (Leaf 3) (Leaf 4)) (Node 1 (Leaf 5) (Leaf 6))]
tail :: BinaryRandomAccessList a -> BinaryRandomAccessList a
tail = snd . uncons

treeSize :: Tree a -> Int
treeSize tree = 2 ^ rank tree

lookupTree :: Tree a -> Int -> a
lookupTree (Leaf value) 0            = value
lookupTree (Node _ left right) index =
    if index < treeSize left then lookupTree left index
    else lookupTree right $ index - treeSize left
lookupTree _ _                       = error "index greater than size of tree"

lookup :: BinaryRandomAccessList a -> Int -> a
lookup (BinaryRandomAccessList []) _                 = error "list is empty"
lookup (BinaryRandomAccessList (first : rest)) index =
    if index < treeSize first then lookupTree first index
    else SparseBinaryRandomAccessList.lookup (BinaryRandomAccessList rest) $ index - treeSize first

updateTree :: Tree a -> Int -> a -> Tree a
updateTree (Leaf _) 0 value                = Leaf value
updateTree (Node r left right) index value =
    if index < treeSize left then Node r (updateTree left index value) right
    else Node r left $ updateTree right (index - treeSize left) value
updateTree _ _ _                           = error "index greater than size of tree"

update :: BinaryRandomAccessList a -> Int -> a -> BinaryRandomAccessList a
update (BinaryRandomAccessList []) _ _                     = error "list is empty"
update (BinaryRandomAccessList (first : rest)) index value =
    if index < treeSize first then BinaryRandomAccessList $ [updateTree first index value] ++ rest
    else consTree first $ update (BinaryRandomAccessList rest) (index - treeSize first) value