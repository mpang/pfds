module SplayHeap where

import Data.Maybe

data SplayTree a =
  Empty | Node (SplayTree a) a (SplayTree a)
  deriving Show

instance Foldable SplayTree where
  foldMap _ Empty                   = mempty
  foldMap f (Node Empty root Empty) = f root
  foldMap f (Node left root right)  = foldMap f left `mappend` f root `mappend` foldMap f right

bigger :: (Ord a) => a -> SplayTree a -> SplayTree a
bigger _ Empty = Empty
bigger pivot (Node left root right)
  | root <= pivot = bigger pivot right
  | otherwise     = case left of
      Empty                            -> Node Empty root right
      Node leftLeft leftRoot leftRight -> if leftRoot <= pivot
        then Node (bigger pivot leftRight) root right
        else Node (bigger pivot leftLeft) leftRoot (Node leftRight root right)

smaller :: (Ord a) => a -> SplayTree a -> SplayTree a
smaller _ Empty = Empty
smaller pivot (Node left root right)
  | root > pivot = smaller pivot left
  | otherwise    = case right of
      Empty                               -> Node left root Empty
      Node rightLeft rightRoot rightRight -> if rightRoot > pivot
        then Node left root $ smaller pivot rightLeft
        else Node (Node left root rightLeft) rightRoot (smaller pivot rightRight)

insert :: (Ord a) => a -> SplayTree a -> SplayTree a
insert element tree = Node (smaller element tree) element (bigger element tree)

findMin :: SplayTree a -> Maybe a
findMin Empty               = Nothing
findMin (Node Empty root _) = Just root
findMin (Node left _ _)     = findMin left

deleteMin :: SplayTree a -> Maybe (SplayTree a)
deleteMin Empty                                                = Nothing
deleteMin (Node Empty _ right)                                 = Just right
deleteMin (Node (Node Empty _ leftRight) root right)           = Just $ Node leftRight root right
deleteMin (Node (Node leftLeft leftRoot leftRight) root right) = Just $ Node (fromJust $ deleteMin leftLeft) leftRoot (Node leftRight root right)

-- | Sort a list by using splay trees
--
-- >>> sort []
-- []
-- >>> sort [1]
-- [1]
-- >>> sort [1, 2, 3, 4, 5]
-- [1,2,3,4,5]
-- >>> sort [5, 4, 3, 2, 1]
-- [1,2,3,4,5]
-- >>> sort [3, 4, 1, 7, 5]
-- [1,3,4,5,7]
-- >>> sort [4, 3, 8, 2, 3, 5, 10, 4, 7]
-- [2,3,3,4,4,5,7,8,10]
sort :: (Ord a) => [a] -> [a]
sort list = sorted
  where tree   = foldr insert Empty list
        sorted = foldMap (: []) tree
