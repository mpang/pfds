module Chapter5.Deques where

data Deque a = Deque [a] [a] deriving Show

empty :: Deque a
empty = Deque [] []

addFirst :: a -> Deque a -> Deque a
addFirst x (Deque [y] [])     = Deque [x] [y]
addFirst x (Deque front rear) = Deque (x : front) rear

getFirst :: Deque a -> Maybe a
getFirst (Deque [] [])   = Nothing
getFirst (Deque [] [x])  = Just x
getFirst (Deque (x:_) _) = Just x

removeFirst :: Deque a -> Maybe (Deque a)
removeFirst (Deque [] [])       = Nothing
removeFirst (Deque [] [_])      = Just (Deque [] [])
removeFirst (Deque [_] rear)    = Just (Deque (reverse newFront) newRear)
  where (newRear, newFront) = splitAt ((length rear + 1) `div` 2) rear
removeFirst (Deque (_:xs) rear) = Just (Deque xs rear)

getLast :: Deque a -> Maybe a
getLast (Deque [] [])   = Nothing
getLast (Deque [x] [])  = Just x
getLast (Deque _ (x:_)) = Just x

addLast :: a -> Deque a -> Deque a
addLast x (Deque [] [y])     = Deque [y] [x]
addLast x (Deque front rear) = Deque front (x : rear)

removeLast :: Deque a -> Maybe (Deque a)
removeLast (Deque [] [])        = Nothing
removeLast (Deque [_] [])       = Just (Deque [] [])
removeLast (Deque front [_])    = Just (Deque newFront (reverse newRear))
  where (newFront, newRear) = splitAt ((length front + 1) `div` 2) front
removeLast (Deque front (_:xs)) = Just (Deque front xs)
