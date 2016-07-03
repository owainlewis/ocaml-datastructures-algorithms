module Data.Algorithms where

import Data.List(insert)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []
