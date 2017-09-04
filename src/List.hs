module List where

-- | Exercise 2.1 - Computes all the suffixes of a list
--
-- >>> suffixes []
-- [[]]
--
-- >>> suffixes [1, 2, 3, 4]
-- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
suffixes :: [a] -> [[a]]
suffixes []       = [[]]
suffixes (x : xs) = (x : xs) : suffixes xs
