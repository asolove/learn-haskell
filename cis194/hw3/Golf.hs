module Golf where

-- Ex 1: skips

skips :: [a] -> [[a]]
skips s = map (`skip` s) [0..length s-1]

skip :: Int -> [a] -> [a]
skip n = map last . partition (n+1)

partition :: Int -> [a] -> [[a]]
partition n xs | length xs < n = []
partition n xs = take n xs : partition n (drop n xs)

-- Ex 2: local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest) | b > a && b > c = b : localMaxima (b:c:rest)
localMaxima (a:b:c:rest) = localMaxima (b:c:rest)
localMaxima _ = []

-- Ex 3: histogram
histogram :: [Integer] -> String
histogram ns =  unlines (reverse rows ++ bars ++ axis)
  where rows = map row [1..maximum itemCounts]
        row r = map (entry r) [0..9]
        entry r c | itemCounts !! c >= r = '*'
        entry _ _ = ' '
        itemCounts = counts ns
        bars = [replicate 10 '=']
        axis = [concatMap show [0..9]]

counts :: [Integer] -> [Int]
counts ns = map count [0..9]
  where count n = length . filter (== n) $ ns
