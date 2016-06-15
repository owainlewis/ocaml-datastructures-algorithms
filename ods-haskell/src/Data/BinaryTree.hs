module Data.BinaryTree where

data Tree a = Leaf | Node (Tree a) a (Tree a)
