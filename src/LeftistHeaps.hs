module LeftistHeaps where

data Heap a =
  Empty | Node Int (Heap a) a (Heap a)
  deriving Show

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

findMin :: Heap a -> Maybe a
findMin Empty             = Nothing
findMin (Node _ _ root _) = Just root

getRank :: Heap a -> Int
getRank Empty             = 0
getRank (Node rank _ _ _) = rank

createHeap :: (Ord a) => a -> Heap a -> Heap a -> Heap a
createHeap root left right
  | getRank left < getRank right = createHeap root right left
  | otherwise                    = Node (getRank right + 1) left root right

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty right = right
merge left Empty  = left
merge left@(Node _ leftLeft leftRoot leftRight) right
  | findMin left > findMin right = merge right left
  | otherwise                    = createHeap leftRoot leftLeft $ merge leftRight right

fromList :: (Ord a) => [a] -> Heap a
fromList elements = go $ map (insert Empty) elements
  where go :: (Ord a) => [Heap a] -> Heap a
        go []         = Empty
        go [result]   = result
        go (x:y:rest) = go $ rest ++ [merge x y]

insert :: (Ord a) => Heap a -> a -> Heap a
insert heap element = merge heap (Node 1 Empty element Empty)

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty                 = Empty
deleteMin (Node _ left _ right) = merge left right
