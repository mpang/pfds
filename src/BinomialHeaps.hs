module BinomialHeaps where

import Data.Maybe

-- children are in decreasing order
data BinomialTree a = Node Int a [BinomialTree a]
  deriving Show

-- trees are in increasing order
newtype BinomialHeap a = BinomialHeap [BinomialTree a]
  deriving Show

makeBinomialTree :: a -> BinomialTree a
makeBinomialTree = flip (Node 0) []

rank :: BinomialTree a -> Int
rank (Node r _ _) = r

root :: BinomialTree a -> a
root (Node _ r _) = r

-- link two tress with the same rank
link :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
link left@(Node rank1 root1 children1) right@(Node _ root2 children2)
  | root1 < root2 = Node (rank1 + 1) root1 (right : children1)
  | otherwise     = Node (rank1 + 1) root2 (left : children2)

insertTree :: (Ord a) => BinomialTree a -> BinomialHeap a -> BinomialHeap a
insertTree tree (BinomialHeap [])     = BinomialHeap [tree]
insertTree tree (BinomialHeap trees@(x:xs))
  | rank tree < rank x = BinomialHeap (tree : trees)
  | otherwise          = insertTree (link tree x) (BinomialHeap xs)

insertElement :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a
insertElement e = insertTree (makeBinomialTree e)

mergeHeap :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
mergeHeap left (BinomialHeap [])  = left
mergeHeap (BinomialHeap []) right = right
mergeHeap left@(BinomialHeap (x:xs)) right@(BinomialHeap (y:ys))
  | rank x < rank y = insertTree x (mergeHeap (BinomialHeap xs) right)
  | rank x > rank y = insertTree y (mergeHeap left (BinomialHeap ys))
  | otherwise       = insertTree (link x y) (mergeHeap (BinomialHeap xs) (BinomialHeap ys))

removeMinTree :: (Ord a) => BinomialHeap a -> Maybe (BinomialTree a, BinomialHeap a)
removeMinTree (BinomialHeap [])     = Nothing
removeMinTree (BinomialHeap [tree]) = Just (tree, BinomialHeap [])
removeMinTree (BinomialHeap (x:xs))
  | root x < root minInRest = Just (x, BinomialHeap xs)
  | otherwise               = Just (minInRest, insertTree x rest)
  where (minInRest, rest) = fromJust $ removeMinTree (BinomialHeap xs)

deleteMin :: (Ord a) => BinomialHeap a -> Maybe (BinomialHeap a)
deleteMin (BinomialHeap []) = Nothing
deleteMin heap              = Just $ mergeHeap (BinomialHeap $ reverse children) rest
  where (Node _ _ children, rest) = fromJust $ removeMinTree heap

findMin :: (Ord a) => BinomialHeap a -> Maybe a
findMin (BinomialHeap [])    = Nothing
findMin (BinomialHeap trees) = Just $ minimum (map root trees)
