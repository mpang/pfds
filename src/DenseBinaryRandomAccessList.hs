module DenseBinaryRandomAccessList where

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

data Digit a = Zero | One (Tree a) deriving Show

newtype BinaryRandomAccessList a = BinaryRandomAccessList [Digit a] deriving Show

empty :: BinaryRandomAccessList a
empty = BinaryRandomAccessList []

isEmpty :: BinaryRandomAccessList a -> Bool
isEmpty (BinaryRandomAccessList digits) = null digits

size :: Tree a -> Int
size (Leaf _)     = 1
size (Node s _ _) = s

link :: Tree a -> Tree a -> Tree a
link left right = Node (size left + size right) left right

consTree :: Tree a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
consTree tree (BinaryRandomAccessList [])                 = BinaryRandomAccessList [One tree]
consTree tree (BinaryRandomAccessList (Zero : ds))        = BinaryRandomAccessList $ [One tree] ++ ds
consTree tree (BinaryRandomAccessList ((One first) : ds)) = BinaryRandomAccessList $ [Zero] ++ newDigits
  where (BinaryRandomAccessList newDigits) = (consTree (link tree first) (BinaryRandomAccessList ds))

-- | Add an item to the random access list
--
-- >>> (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 (cons 17 (cons 18 (cons 19 (cons 20 (cons 21 (cons 22 (cons 23 empty))))))))))))))
-- BinaryRandomAccessList [Zero,One (Node 2 (Leaf 10) (Leaf 11)),One (Node 4 (Node 2 (Leaf 12) (Leaf 13)) (Node 2 (Leaf 14) (Leaf 15))),One (Node 8 (Node 4 (Node 2 (Leaf 16) (Leaf 17)) (Node 2 (Leaf 18) (Leaf 19))) (Node 4 (Node 2 (Leaf 20) (Leaf 21)) (Node 2 (Leaf 22) (Leaf 23))))]
cons :: a -> BinaryRandomAccessList a -> BinaryRandomAccessList a
cons = consTree . Leaf

uncons :: BinaryRandomAccessList a -> (Tree a, BinaryRandomAccessList a)
uncons (BinaryRandomAccessList [One tree])        = (tree, BinaryRandomAccessList [])
uncons (BinaryRandomAccessList ((One tree) : ds)) = (tree, BinaryRandomAccessList ds)
uncons (BinaryRandomAccessList (Zero : ds))       = (left, BinaryRandomAccessList $ [One right] ++ newDigits)
  where ((Node _ left right), (BinaryRandomAccessList newDigits)) = uncons $ BinaryRandomAccessList ds

head :: BinaryRandomAccessList a -> a
head list = let (Leaf value) = fst (uncons list) in value

tail :: BinaryRandomAccessList a -> BinaryRandomAccessList a
tail = snd . uncons

lookupTree :: Tree a -> Int -> a
lookupTree (Leaf value) 0            = value
lookupTree (Node s left right) index =
  if index < s `div` 2 then lookupTree left index
  else lookupTree right (index - s `div` 2)

lookup :: BinaryRandomAccessList a -> Int -> a
lookup (BinaryRandomAccessList (Zero : ds)) index       = DenseBinaryRandomAccessList.lookup (BinaryRandomAccessList ds) index
lookup (BinaryRandomAccessList ((One tree) : ds)) index =
  if index < size tree then lookupTree tree index
  else DenseBinaryRandomAccessList.lookup (BinaryRandomAccessList ds) (index - size tree)

updateTree :: Tree a -> Int -> a -> Tree a
updateTree (Leaf _) 0 newValue                = Leaf newValue
updateTree (Node s left right) index newValue =
  if index < s `div` 2 then Node s (updateTree left index newValue) right
  else Node s left $ updateTree right (index - s `div` 2) newValue

update :: BinaryRandomAccessList a -> Int -> a -> BinaryRandomAccessList a
update (BinaryRandomAccessList (Zero : ds)) index newValue             = BinaryRandomAccessList $ [Zero] ++ newDigits
  where (BinaryRandomAccessList newDigits) = update (BinaryRandomAccessList ds) index newValue
update (BinaryRandomAccessList (first@(One tree) : ds)) index newValue =
  if index < size tree then BinaryRandomAccessList $ [One (updateTree tree index newValue)] ++ ds
  else BinaryRandomAccessList $ [first] ++ newDigits
    where (BinaryRandomAccessList newDigits) = update (BinaryRandomAccessList ds) (index - size tree) newValue

fixupHelper :: [Tree a] -> Int -> Bool -> [Digit a] -> Int -> [Digit a]
fixupHelper trees num shouldAppendZero result currentIndex =
  if num == currentIndex then result
  else case trees of
    (first : rest)
      | size first == 2 ^ currentIndex -> fixupHelper rest num shouldAppendZero (result ++ [One first]) (currentIndex + 1)
      | size first > 2 ^ currentIndex  -> fixupHelper trees num shouldAppendZero ([Zero] ++ result) (currentIndex + 1)
    [] -> if shouldAppendZero then result ++ (replicate (num - currentIndex) Zero) else result

fixup :: [Tree a] -> Int -> Bool -> [Digit a]
fixup trees lastIndex shouldAppendZero = fixupHelper trees (lastIndex + 1) shouldAppendZero [] 0

dropTree :: Tree a -> Int -> [Tree a]
dropTree tree 0                = [tree]
dropTree (Node s left right) k =
  if k < s `div` 2 then dropTree left k ++ [right]
  else dropTree right $ k - s `div` 2

dropHelper :: BinaryRandomAccessList a -> Int -> Int -> BinaryRandomAccessList a
dropHelper (BinaryRandomAccessList []) 0 _                    = empty
dropHelper (BinaryRandomAccessList digits) 0 index            = BinaryRandomAccessList $ replicate index Zero ++ digits
dropHelper (BinaryRandomAccessList (Zero : ds)) k index       = dropHelper (BinaryRandomAccessList ds) k (index + 1)
dropHelper (BinaryRandomAccessList ((One tree) : ds)) k index =
  if k < size tree then BinaryRandomAccessList $ fixup (dropTree tree k) index (not $ null ds) ++ ds
  else dropHelper (BinaryRandomAccessList ds) (k - size tree) (index + 1)

-- | Drops the first k elements from a random access list
--
-- >>> DenseBinaryRandomAccessList.drop empty 0
-- BinaryRandomAccessList []
--
-- >>> DenseBinaryRandomAccessList.drop (cons 10 empty) 1
-- BinaryRandomAccessList []
--
-- >>> DenseBinaryRandomAccessList.drop (cons 10 (cons 11 empty)) 1
-- BinaryRandomAccessList [One (Leaf 11)]
--
-- >>> DenseBinaryRandomAccessList.drop (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 empty)))))) 3
-- BinaryRandomAccessList [One (Leaf 13),One (Node 2 (Leaf 14) (Leaf 15))]
--
-- >>> DenseBinaryRandomAccessList.drop (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 empty)))))) 2
-- BinaryRandomAccessList [Zero,Zero,One (Node 4 (Node 2 (Leaf 12) (Leaf 13)) (Node 2 (Leaf 14) (Leaf 15)))]
--
-- >>> DenseBinaryRandomAccessList.drop (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 (cons 17 (cons 18 (cons 19 (cons 20 (cons 21 (cons 22 (cons 23 empty)))))))))))))) 3
-- BinaryRandomAccessList [One (Leaf 13),One (Node 2 (Leaf 14) (Leaf 15)),Zero,One (Node 8 (Node 4 (Node 2 (Leaf 16) (Leaf 17)) (Node 2 (Leaf 18) (Leaf 19))) (Node 4 (Node 2 (Leaf 20) (Leaf 21)) (Node 2 (Leaf 22) (Leaf 23))))]
--
drop :: BinaryRandomAccessList a -> Int -> BinaryRandomAccessList a
drop list k = dropHelper list k 0
