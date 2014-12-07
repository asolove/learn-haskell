module Golf where

-- Ex 1: skips

skips :: [a] -> [[a]]
skips s = map (`skip` s) [0..length s-1]

skip :: Int -> [a] -> [a]
skip n = map last . partition (n+1)

partition :: Int -> [a] -> [[a]]
partition n xs | length xs < n = []
partition n xs = take n xs : partition n (drop n xs)

