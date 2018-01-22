module SkewBinomialHeap where

data Tree a = Node Int a [a] [Tree a]

newtype BinomialHeap a = BinomialHeap [Tree a]

treeRank :: Tree a -> Int
treeRank (Node r _ _ _) = r

treeRoot :: Tree a -> a
treeRoot (Node _ r _ _) = r

link :: (Ord a) => Tree a -> Tree a -> Tree a
link tree1@(Node rank1 root1 list1 children1) tree2@(Node _ root2 list2 children2) =
    if root1 < root2 then Node (rank1 + 1) root1 list1 $ [tree2] ++ children1
    else Node (rank1 + 1) root2 list2 $ [tree1] ++ children2

skewLink :: (Ord a) => a -> Tree a -> Tree a -> Tree a
skewLink element tree1 tree2 = let (Node rank root list children) = link tree1 tree2 in
    if element < root then Node rank element ([root] ++ list) children
    else Node rank root ([element] ++ list) children

insert :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a
insert element (BinomialHeap (first : second : rest)) =
    if treeRank first == treeRank second then BinomialHeap $ [(skewLink element first second)] ++ rest
    else BinomialHeap $ [Node 0 element [] []] ++ rest
insert element (BinomialHeap trees)                   = BinomialHeap $ [Node 0 element [] []] ++ trees

insertTree :: (Ord a) => Tree a -> BinomialHeap a -> BinomialHeap a
insertTree tree (BinomialHeap [])                   = BinomialHeap [tree]
insertTree tree (BinomialHeap trees@(first : rest)) =
    if treeRank tree < treeRank first then BinomialHeap $ [tree] ++ trees
    else insertTree (link tree first) $ BinomialHeap rest

normalize :: (Ord a) => BinomialHeap a -> BinomialHeap a
normalize (BinomialHeap [])             = BinomialHeap []
normalize (BinomialHeap (first : rest)) = insertTree first $ BinomialHeap rest

mergeHelper :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
mergeHelper (BinomialHeap []) right = right
mergeHelper left (BinomialHeap [])  = left
mergeHelper left@(BinomialHeap (x : xs)) right@(BinomialHeap (y : ys))
    | treeRank x < treeRank y = insertTree x $ mergeHelper (BinomialHeap xs) right
    | treeRank y < treeRank x = insertTree y $ mergeHelper left (BinomialHeap ys)
    | otherwise               = insertTree (link x y) $ mergeHelper (BinomialHeap xs) (BinomialHeap ys)

merge :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge first second = mergeHelper (normalize first) (normalize second)

removeMinTree :: (Ord a) => BinomialHeap a -> (Tree a, BinomialHeap a)
removeMinTree (BinomialHeap [])             = error "heap is empty"
removeMinTree (BinomialHeap [tree])         = (tree, BinomialHeap [])
removeMinTree (BinomialHeap (first : rest)) =
    if treeRoot first < treeRoot minInRest then (first, BinomialHeap rest)
    else (minInRest, insertTree first newRest)
        where (minInRest, newRest) = removeMinTree $ BinomialHeap rest

findMin :: (Ord a) => BinomialHeap a -> a
findMin = treeRoot . fst . removeMinTree