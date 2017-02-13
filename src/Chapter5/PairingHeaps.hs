module Chapter5.PairingHeaps where

data PairingHeap a = Empty | Node a [PairingHeap a]
  deriving Show

findMin :: PairingHeap a -> Maybe a
findMin Empty         = Nothing
findMin (Node root _) = Just root

merge :: (Ord a) => PairingHeap a -> PairingHeap a -> PairingHeap a
merge left Empty  = left
merge Empty right = right
merge left@(Node leftRoot leftChildren) right@(Node rightRoot rightChildren)
  | leftRoot < rightRoot = Node leftRoot $ right : leftChildren
  | otherwise            = Node rightRoot $ left : rightChildren

insert :: (Ord a) => a -> PairingHeap a -> PairingHeap a
insert element = merge (Node element [])

mergePairs :: (Ord a) => [PairingHeap a] -> PairingHeap a
mergePairs []      = Empty
mergePairs [heap]  = heap
-- we merge them in two passes. In the first pass we merge from
-- left to right in pairs; in the second pass we merge from right
-- to left. We achieve it through recursion.
mergePairs (x:y:z) = merge (merge x y) (mergePairs z)

deleteMin :: (Ord a) => PairingHeap a -> Maybe (PairingHeap a)
deleteMin Empty             = Nothing
deleteMin (Node _ children) = Just $ mergePairs children

data BinaryTree a = Empty' | Node' (BinaryTree a) a (BinaryTree a)
  deriving Show

toBinaryTree :: PairingHeap a -> BinaryTree a
toBinaryTree Empty = Empty'
toBinaryTree heap  = toBinaryTreeFromList [heap]
  where toBinaryTreeFromList []                            = Empty'
        toBinaryTreeFromList (Node root children:siblings) = Node' (toBinaryTreeFromList children) root (toBinaryTreeFromList siblings)
        toBinaryTreeFromList (Empty:_)                     = error "Bug: Empty appears in list of children"
