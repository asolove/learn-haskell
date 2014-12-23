module HW4 where

import Data.List (transpose, sort, (\\))

-- Ex 2: Tree

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

insert :: (Ord a) => a -> Tree a -> Tree a
insert n Leaf = Node 1 Leaf n Leaf
insert n (Node h l v r) | n > v = Node h' l v r'
  where r' = insert n r
        h' = 1 + height r'
insert n (Node h l v r) = Node h' l' v r
  where l' = insert n l
        h' = 1 + height l'

-- naively inserts values into the three
foldTreeDumb :: (Ord a) => [a] -> Tree a
foldTreeDumb = foldr insert Leaf

merge :: [[a]] -> [a]
merge = concat . transpose 

-- given a sorted list, return a new list with the middle
-- e.g. [1, 2, 3, 4, 5, 6, 7] -> [4, 2, 6, 1, 3, 5, 7]
fractalUp :: [a] -> [a]
fractalUp [] = []
fractalUp xs = [middle] ++ merge [fractalUp left, fractalUp right]
  where middle = xs !! m
        m = length xs `div` 2
        left = take m xs
        right = drop (m+1) xs

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldTreeDumb . fractalUp . sort


-- Ex 3: folds

xor :: [Bool] -> Bool
xor [] = False
xor (False:xs) = not (xor xs)
xor (True:xs) = xor xs

map' :: (a -> b) -> [a] -> [b]
map' fn as = foldr step [] as
  where step a l = [fn a] ++ l

-- Ex 4. Sieve of Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n] \\ removals]
  where removals = [i+j+2*i*j | j <- [1..n], i <- [1..n]]
