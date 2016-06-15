module Data.BinaryHeap where

data BinaryHeap a = Leaf | Node a (BinaryHeap a) (BinaryHeap a)
    deriving (Eq, Ord, Show)

isEmpty :: BinaryHeap a -> Bool
isEmpty Leaf = True
isEmpty (Node _ _ _) = False

empty :: BinaryHeap a
empty = Leaf
