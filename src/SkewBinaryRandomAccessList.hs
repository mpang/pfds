module SkewBinaryRandomAccessList where

data Tree a = Leaf a | Node a (Tree a) (Tree a)

newtype BinaryRandomAccessList a = BinaryRandomAccessList [(Int, Tree a)]

empty :: BinaryRandomAccessList a
empty = BinaryRandomAccessList []

cons :: BinaryRandomAccessList a -> a -> BinaryRandomAccessList a
cons (BinaryRandomAccessList list@((size1, tree1) : (size2, tree2) : rest)) element =
    if size1 == size2 then BinaryRandomAccessList $ [(1 + size1 + size2, Node element tree1 tree2)] ++ rest
    else BinaryRandomAccessList $ [(1, Leaf element)] ++ list
cons (BinaryRandomAccessList list) element                                          = BinaryRandomAccessList $ [(1, Leaf element)] ++ list

head :: BinaryRandomAccessList a -> a
head (BinaryRandomAccessList ((_, Leaf value) : _))     = value
head (BinaryRandomAccessList ((_, Node value _ _) : _)) = value

tail :: BinaryRandomAccessList a -> BinaryRandomAccessList a
tail (BinaryRandomAccessList ((_, Leaf _) : rest))               = BinaryRandomAccessList rest
tail (BinaryRandomAccessList ((size, Node _ left right) : rest)) = BinaryRandomAccessList $ [(size `div` 2, left), (size `div` 2, right)] ++ rest

lookupTree :: Tree a -> Int -> Int -> a
lookupTree (Leaf value) _ 0                   = value
lookupTree (Node root _ _) _ 0                = root
lookupTree (Node _ left right) treeSize index =
    if index <= treeSize `div` 2 then lookupTree left (treeSize `div` 2) (index - 1)
    else lookupTree right (treeSize `div` 2) (index - 1 - treeSize `div` 2)
lookupTree _ _ _                              = error "index greater than tree size"

lookup :: BinaryRandomAccessList a -> Int -> a
lookup (BinaryRandomAccessList ((firstSize, firstTree) : rest)) index =
    if index < firstSize then lookupTree firstTree firstSize index
    else SkewBinaryRandomAccessList.lookup (BinaryRandomAccessList rest) $ index - firstSize
