module ZerolessBinaryRandomAccessList where

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

data Digit a = One (Tree a) | Two (Tree a) (Tree a) deriving Show

newtype BinaryRandomAccessList a = BinaryRandomAccessList [Digit a] deriving Show

empty :: BinaryRandomAccessList a
empty = BinaryRandomAccessList []

treeSize :: Tree a -> Int
treeSize (Leaf _)        = 1
treeSize (Node size _ _) = size

link :: Tree a -> Tree a -> Tree a
link left right = Node (treeSize left + treeSize right) left right

consTree :: Tree a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
consTree tree (BinaryRandomAccessList [])                          = BinaryRandomAccessList [One tree]
consTree tree (BinaryRandomAccessList ((One first) : rest))        = BinaryRandomAccessList $ [Two tree first] ++ rest
consTree tree (BinaryRandomAccessList ((Two first second) : rest)) = BinaryRandomAccessList $ [One tree] ++ newDigits
    where (BinaryRandomAccessList newDigits) = consTree (link first second) $ BinaryRandomAccessList rest

-- | Adds an element to the start of the list
--
-- >>> cons 1 empty
-- BinaryRandomAccessList [One (Leaf 1)]
--
-- >>> cons 1 (cons 2 empty)
-- BinaryRandomAccessList [Two (Leaf 1) (Leaf 2)]
--
-- >>> cons 1 (cons 2 (cons 3 empty))
-- BinaryRandomAccessList [One (Leaf 1),One (Node 2 (Leaf 2) (Leaf 3))]
--
-- >>> cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 empty)))))))))))))))
-- BinaryRandomAccessList [Two (Leaf 1) (Leaf 2),One (Node 2 (Leaf 3) (Leaf 4)),One (Node 4 (Node 2 (Leaf 5) (Leaf 6)) (Node 2 (Leaf 7) (Leaf 8))),One (Node 8 (Node 4 (Node 2 (Leaf 9) (Leaf 10)) (Node 2 (Leaf 11) (Leaf 12))) (Node 4 (Node 2 (Leaf 13) (Leaf 14)) (Node 2 (Leaf 15) (Leaf 16))))]
cons :: a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
cons = consTree . Leaf

head :: BinaryRandomAccessList a -> a
head (BinaryRandomAccessList [])                                = error "list is empty"
head (BinaryRandomAccessList ((One (Leaf value)) : _))          = value
head (BinaryRandomAccessList ((Two (Leaf value) (Leaf _)) : _)) = value
head _                                                          = error "invalid list structure"

tail :: BinaryRandomAccessList a -> BinaryRandomAccessList a
tail (BinaryRandomAccessList [])                                          = error "list is empty"
tail (BinaryRandomAccessList ((One (Leaf _)) : rest))                     = BinaryRandomAccessList rest
tail (BinaryRandomAccessList ((Two (Leaf _) secondLeaf@(Leaf _)) : rest)) = BinaryRandomAccessList $ [One secondLeaf] ++ rest
tail _                                                                    = error "invalid list structure"

lookupTree :: Tree a -> Int -> a
lookupTree (Leaf value) 0            = value
lookupTree (Node _ left right) index =
    if index < treeSize left then lookupTree left index
    else lookupTree right $ index - treeSize left
lookupTree _ _                       = error "index greater than tree size"

lookup :: BinaryRandomAccessList a -> Int -> a
lookup (BinaryRandomAccessList []) _ = error "list is empty"
lookup (BinaryRandomAccessList ((One tree) : rest)) index =
    if index < treeSize tree then lookupTree tree index
    else ZerolessBinaryRandomAccessList.lookup (BinaryRandomAccessList rest) $ index - treeSize tree
lookup (BinaryRandomAccessList ((Two first second) : rest)) index
    | index < treeSize first                   = lookupTree first index
    | index < treeSize first + treeSize second = lookupTree second $ index - treeSize first
    | otherwise                                = ZerolessBinaryRandomAccessList.lookup (BinaryRandomAccessList rest) $ index - treeSize first - treeSize second

updateTree :: Tree a -> Int -> a -> Tree a
updateTree (Leaf _) 0 value = Leaf value
updateTree (Node size left right) index value =
    if index < treeSize left then Node size (updateTree left index value) right
    else Node size left $ updateTree right (index - treeSize left) value

update :: BinaryRandomAccessList a -> Int -> a -> BinaryRandomAccessList a
update (BinaryRandomAccessList []) _ _ = error "list is empty"
update (BinaryRandomAccessList ((One tree) : rest)) index value =
    if index < treeSize tree then BinaryRandomAccessList $ [One $ updateTree tree index value] ++ rest
    else ZerolessBinaryRandomAccessList.update (BinaryRandomAccessList rest) (index - treeSize tree) value
update (BinaryRandomAccessList (two@(Two first second) : rest)) index value
    | index < treeSize first                   = BinaryRandomAccessList $ [Two (updateTree first index value) second] ++ rest
    | index < treeSize first + treeSize second = BinaryRandomAccessList $ [Two first $ updateTree second (index - treeSize first) value] ++ rest
    | otherwise                                = BinaryRandomAccessList $ [two] ++ newDigits
        where (BinaryRandomAccessList newDigits) = update (BinaryRandomAccessList rest) (index - treeSize first - treeSize second) value