{-# LANGUAGE DeriveFunctor #-}
module Data.BinaryTree where

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Eq, Ord, Show, Functor)

empty :: Tree a
empty = Leaf

terminal :: a -> Tree a
terminal n = Node Leaf n Leaf
