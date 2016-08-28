module Chapter2.BinarySearchTrees where

import Data.Maybe

data BinarySearchTree a =
  Leaf | Node (BinarySearchTree a) a (BinarySearchTree a)
  deriving Show

-- | Returns an empty binary search tree
--
-- >>> empty
-- Leaf
empty :: BinarySearchTree a
empty = Leaf

-- | Tests whether a binary search tree is empty
--
-- >>> isEmpty Leaf
-- True
--
-- >> isEmpty (Node Leaf 0 Leaf)
-- False
isEmpty :: BinarySearchTree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Tests whether an element is a member of a tree (takes 2d comparisons where d is the depth of the tree)
--
-- >>> isMember 1 Leaf
-- False
--
-- >>> isMember 1 (Node Leaf 1 Leaf)
-- True
--
-- >>> isMember 1 (Node Leaf 2 Leaf)
-- False
--
-- >>> isMember 1 (Node (Node Leaf 1 Leaf) 3 Leaf)
-- True
isMember :: (Ord a) => a -> BinarySearchTree a -> Bool
isMember _ Leaf = False
isMember x (Node left value right)
  | x < value = isMember x left
  | x > value = isMember x right
  | otherwise = True

-- | Exercise 2.2 - Tests whether an element is a member of a tree (takes d + 1 comparisons where d is the depth of the tree)
--
-- >>> isMember 1 Leaf
-- False
--
-- >>> isMember 1 (Node Leaf 1 Leaf)
-- True
--
-- >>> isMember 1 (Node Leaf 2 Leaf)
-- False
--
-- >>> isMember 1 (Node (Node Leaf 1 Leaf) 3 Leaf)
-- True
isMember2 :: (Ord a) => a -> BinarySearchTree a -> Bool
isMember2 x tree = go x tree Nothing
  where go :: (Ord a) => a -> BinarySearchTree a -> Maybe a -> Bool
        go _ Leaf Nothing           = False
        go x' Leaf (Just candidate) = x' == candidate
        go x' (Node left value right) candidate
          | x' < value = go x' left candidate
          | otherwise  = go x' right (Just value)

-- | Inserts an element into a tree
--
-- >>> insert 1 Leaf
-- Node Leaf 1 Leaf
--
-- >>> insert 1 (Node Leaf 2 Leaf)
-- Node (Node Leaf 1 Leaf) 2 Leaf
--
-- >>> insert 3 (Node Leaf 2 Leaf)
-- Node Leaf 2 (Node Leaf 3 Leaf)
--
-- >>> insert 4 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf))
-- Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 5 Leaf)
--
-- >>> insert 5 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf))
-- Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf)
insert :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left value right)
  | x < value = Node (insert x left) value right
  | x > value = Node left value (insert x right)
  | otherwise = Node left value right

-- | Exercise 2.3 - Inserts an element into a tree without copying the search path
--                  when inserting an already existing element
--
-- >>> insert2 1 Leaf
-- Node Leaf 1 Leaf
--
-- >>> insert2 1 (Node Leaf 2 Leaf)
-- Node (Node Leaf 1 Leaf) 2 Leaf
--
-- >>> insert2 3 (Node Leaf 2 Leaf)
-- Node Leaf 2 (Node Leaf 3 Leaf)
--
-- >>> insert2 4 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf))
-- Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 5 Leaf)
--
-- >>> insert2 5 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf))
-- Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf)
insert2 :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
insert2 x tree = fromMaybe tree (go x tree)
  where go :: (Ord a) => a -> BinarySearchTree a -> Maybe (BinarySearchTree a)
        go x' Leaf = Just (Node Leaf x' Leaf)
        go x' (Node left value right)
          | x' < value = Node <$> go x' left <*> Just value <*> Just right
          | x' > value = fmap (Node left value) (go x' right)
          | otherwise  = Nothing

-- | Exercise 2.4 - Inserts an element into a tree without copying the search path
--                  when inserting an already existing element. Also it only does
--                  d + 1 comparisons where d is the depth of the tree
--
-- >>> insert3 1 Leaf
-- Node Leaf 1 Leaf
--
-- >>> insert3 1 (Node Leaf 2 Leaf)
-- Node (Node Leaf 1 Leaf) 2 Leaf
--
-- >>> insert3 3 (Node Leaf 2 Leaf)
-- Node Leaf 2 (Node Leaf 3 Leaf)
--
-- >>> insert3 4 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf))
-- Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 5 Leaf)
--
-- >>> insert3 5 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf))
-- Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf)
insert3 :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
insert3 x tree = fromMaybe tree (go x tree Nothing)
  where go :: (Ord a) => a -> BinarySearchTree a -> Maybe a -> Maybe (BinarySearchTree a)
        go x' Leaf Nothing = Just (Node Leaf x' Leaf)
        go x' Leaf (Just candidate) = if candidate == x' then Nothing else Just (Node Leaf x' Leaf)
        go x' (Node left value right) candidate
          | x' < value = Node <$> go x' left candidate <*> Just value <*> Just right
          | otherwise  = fmap (Node left value) (go x' right (Just value))

-- | Exercise 2.5 a - Create a complete binary search tree with given depth and value
--
-- >>> complete 5 0
-- Leaf
--
-- >>> complete 5 1
-- Node Leaf 5 Leaf
--
-- >>> complete 5 3
-- Node (Node (Node Leaf 5 Leaf) 5 (Node Leaf 5 Leaf)) 5 (Node (Node Leaf 5 Leaf) 5 (Node Leaf 5 Leaf))
complete :: a -> Int -> BinarySearchTree a
complete _ 0     = Leaf
complete value n = Node subtree value subtree
  where subtree = complete value (n - 1)

createBalance :: a -> Int -> BinarySearchTree a
createBalance value size
  | size == 0       = Leaf
  | size == 1       = Node Leaf value Leaf
  | even (size - 1) =
    let subtree = createBalance value ((size - 1) `quot` 2)
      in Node subtree value subtree
  | otherwise =
    let (leftTree, rightTree) = createBalance2 ((size `quot` 2) - 1)
        createBalance2 size'  = (createBalance value size', createBalance value (size' + 1))
      in Node leftTree value rightTree

-- | Delete an element from a tree
--
-- >>> delete 1 Leaf
-- Just Leaf
--
-- >>> delete 1 (Node Leaf 3 Leaf)
-- Just (Node Leaf 3 Leaf)
--
-- >>> delete 1 (Node Leaf 1 Leaf)
-- Just Leaf
--
-- >>> delete 1 (Node (Node Leaf 1 Leaf) 3 Leaf)
-- Just (Node Leaf 3 Leaf)
--
-- >>> delete 3 (Node (Node Leaf 1 Leaf) 3 (Node (Node Leaf 4 Leaf) 5 Leaf))
-- Just (Node (Node Leaf 1 Leaf) 4 (Node Leaf 5 Leaf))
delete :: (Ord a) => a -> BinarySearchTree a -> Maybe (BinarySearchTree a)
delete _ Leaf = Just Leaf
delete x (Node left value right)
  | x < value = Node <$> delete x left <*> Just value <*> Just right
  | x > value = fmap (Node left value) (delete x right)
  | otherwise = deleteRoot (Node left value right)

deleteRoot :: (Ord a) => BinarySearchTree a -> Maybe (BinarySearchTree a)
deleteRoot Leaf                = Nothing
deleteRoot (Node Leaf _ Leaf)  = Just Leaf
deleteRoot (Node Leaf _ right) = Just right
deleteRoot (Node left _ Leaf)  = Just left
deleteRoot (Node left _ right) = do
  inOrderSuccessor <- leftiest right
  newRight <- delete inOrderSuccessor right
  return $ Node left inOrderSuccessor newRight

leftiest :: BinarySearchTree a -> Maybe a
leftiest Leaf                = Nothing
leftiest (Node Leaf value _) = Just value
leftiest (Node left _ _)     = leftiest left
