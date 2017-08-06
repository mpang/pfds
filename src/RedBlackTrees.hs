module RedBlackTrees where

data Color = Red | Black

data Tree a =
  Empty | Node Color (Tree a) a (Tree a)

balance :: Tree a -> Tree a
balance (Node Black (Node Red a x (Node Red b y c)) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red (Node Red a x b) y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d))) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance tree = tree

insertHelper :: (Ord a) => a -> Tree a -> Tree a
insertHelper x Empty = Node Red Empty x Empty
insertHelper x tree@(Node color left root right)
  | x == root = tree
  | x < root  = Node color (balance $ insertHelper x left) root right
  | otherwise = Node color left root (balance $ insertHelper x right)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x tree = Node Black left root right
  where (Node _ left root right) = insertHelper x tree
