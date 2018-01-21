module SegmentedBinaryRandomAccessList where

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

data Digits a = Zero | Ones [Tree a] | Two (Tree a) (Tree a) deriving Show

newtype BinaryRandomAccessList a = BinaryRandomAccessList [Digits a] deriving Show

empty :: BinaryRandomAccessList a
empty = BinaryRandomAccessList []

treeSize :: Tree a -> Int
treeSize (Leaf _)     = 1
treeSize (Node s _ _) = s

link :: Tree a -> Tree a -> Tree a
link left right = Node (treeSize left + treeSize right) left right

fixup :: BinaryRandomAccessList a -> BinaryRandomAccessList a
fixup (BinaryRandomAccessList ((Two first second) : rest))                       = BinaryRandomAccessList $ [Zero] ++ newDigits
    where (BinaryRandomAccessList newDigits) = consTree (link first second) (BinaryRandomAccessList rest)
fixup (BinaryRandomAccessList (firstOnes@(Ones _) : (Two firstTwo secondTwo) : rest)) = BinaryRandomAccessList $ [firstOnes] ++ [Zero] ++ newDigits
    where (BinaryRandomAccessList newDigits) = consTree (link firstTwo secondTwo) (BinaryRandomAccessList rest)
fixup list                                                                       = list

ones :: [Tree a] -> [Digits a] -> [Digits a]
ones [] digits                       = digits
ones trees ((Ones firstOnes) : rest) = [Ones $ trees ++ firstOnes] ++ rest
ones trees digits                    = [Ones trees] ++ digits

consTree :: Tree a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
consTree tree (BinaryRandomAccessList [])                                    = BinaryRandomAccessList [Ones [tree]]
consTree tree (BinaryRandomAccessList (Zero : rest))                         = BinaryRandomAccessList $ ones [tree] rest
consTree tree (BinaryRandomAccessList ((Ones (firstOne : restOnes)) : rest)) = BinaryRandomAccessList $ [Two tree firstOne] ++ ones restOnes rest
consTree _ _                                                                 = error "leading two in list"

-- | Adds an element to the start of the list
--
-- >>> cons 1 empty
-- BinaryRandomAccessList [Ones [Leaf 1]]
--
-- >>> cons 1 (cons 2 empty)
-- BinaryRandomAccessList [Zero,Ones [Node 2 (Leaf 1) (Leaf 2)]]
--
-- >>> cons 1 (cons 2 (cons 3 empty))
-- BinaryRandomAccessList [Ones [Leaf 1,Node 2 (Leaf 2) (Leaf 3)]]
--
-- >>> cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))
-- BinaryRandomAccessList [Ones [Leaf 1],Zero,Ones [Node 4 (Node 2 (Leaf 2) (Leaf 3)) (Node 2 (Leaf 4) (Leaf 5))]]
--
-- >>> cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty)))))
-- BinaryRandomAccessList [Zero,Ones [Node 2 (Leaf 1) (Leaf 2),Node 4 (Node 2 (Leaf 3) (Leaf 4)) (Node 2 (Leaf 5) (Leaf 6))]]
cons :: a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
cons element list = fixup $ consTree (Leaf element) list

addOneZero :: [Digits a] -> [Digits a]
addOneZero []     = []
addOneZero digits = [Zero] ++ digits

uncons :: BinaryRandomAccessList a -> (Tree a, BinaryRandomAccessList a)
uncons (BinaryRandomAccessList [])                                 = error "list is empty"
uncons (BinaryRandomAccessList (Zero : rest))                      = (left, BinaryRandomAccessList $ ones [right] remaining)
    where ((Node _ left right), (BinaryRandomAccessList remaining)) = uncons $ BinaryRandomAccessList rest
uncons (BinaryRandomAccessList ((Ones (first : restOnes)) : rest)) = (first, BinaryRandomAccessList $ addOneZero $ ones restOnes rest)
uncons _                                                           = error "leading two in a list"

-- | Returns the first element of a list
--
-- >>> SegmentedBinaryRandomAccessList.head (cons 1 empty)
-- 1
--
-- >>> SegmentedBinaryRandomAccessList.head (cons 1 (cons 2 empty))
-- 1
--
-- >>> SegmentedBinaryRandomAccessList.head (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
-- 1
--
-- >>> SegmentedBinaryRandomAccessList.head (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty))))))
-- 1
head :: BinaryRandomAccessList a -> a
head list = case fst $ uncons list of
    (Leaf value) -> value
    _            -> error "invalid uncons operation"

-- | Returns the tail element of a list
--
-- >>> SegmentedBinaryRandomAccessList.tail (cons 1 empty)
-- BinaryRandomAccessList []
--
-- >>> SegmentedBinaryRandomAccessList.tail (cons 1 (cons 2 empty))
-- BinaryRandomAccessList [Ones [Leaf 2]]
--
-- >>> SegmentedBinaryRandomAccessList.tail (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
-- BinaryRandomAccessList [Zero,Zero,Ones [Node 4 (Node 2 (Leaf 2) (Leaf 3)) (Node 2 (Leaf 4) (Leaf 5))]]
--
-- >>> SegmentedBinaryRandomAccessList.tail (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty))))))
-- BinaryRandomAccessList [Ones [Leaf 2],Zero,Ones [Node 4 (Node 2 (Leaf 3) (Leaf 4)) (Node 2 (Leaf 5) (Leaf 6))]]
tail :: BinaryRandomAccessList a -> BinaryRandomAccessList a
tail = snd . uncons

lookupTree :: Tree a -> Int -> a
lookupTree (Leaf value) 0            = value
lookupTree (Node _ left right) index =
    if index < treeSize left then lookupTree left index
    else lookupTree right $ index - treeSize left
lookupTree _ _                       = error "index greater than tree size"

lookupTrees :: [Tree a] -> Int -> a
lookupTrees [] _                 = error "list is empty"
lookupTrees (first : rest) index =
    if index < treeSize first then lookupTree first index
    else lookupTrees rest $ index - treeSize first

lookup :: BinaryRandomAccessList a -> Int -> a
lookup (BinaryRandomAccessList []) _                       = error "list is empty"
lookup (BinaryRandomAccessList (Zero : rest)) index        = SegmentedBinaryRandomAccessList.lookup (BinaryRandomAccessList rest) index
lookup (BinaryRandomAccessList ((Ones firstOnes) : rest)) index =
    if index < sum (map treeSize firstOnes) then lookupTrees firstOnes index
    else SegmentedBinaryRandomAccessList.lookup (BinaryRandomAccessList rest) (index - (sum $ map treeSize firstOnes))
lookup _ _                                                 = error "invalid list structure"