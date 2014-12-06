{-# OPTIONS_GHC -Wall #-}

module HW1 where

-- Ex 1
toDigits :: Integer -> [Integer]
toDigits n | n < 0 = []
toDigits n = map read . map (:[]) . show $ n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther'. reverse
  where doubleEveryOther' [] = []
        doubleEveryOther' [x] = [x]
        doubleEveryOther' (x:y:ys) = x : y*2 : doubleEveryOther' ys

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Ex 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits


-- Tower of Hanoi
-- Ex 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = moveAbove ++ [moveBottom] ++ moveAboveBack
  where moveAbove = hanoi (n-1) a c b
        moveBottom = (a, b)
        moveAboveBack = hanoi (n-1) c b a

-- Ex 6: Hanoi with four pegs
-- Can be done in 129. My solution takes 382.
-- To move from a to b using c and d as storage
-- Move n-2 from a to d using b and c
-- Move 1 from a to c
-- Move 1 from a to b

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 0 _ _ _ _ = []
hanoiFour 1 a b _ _ = [(a, b)]
hanoiFour n a b c d = moveAbove ++ [movePenultimate] ++ [moveBottom] ++ moveAboveBack
  where moveAbove = hanoiFour (n-2) a d b c
        movePenultimate = (a, c)
        moveBottom = (a, b)
        moveAboveBack = hanoiFour (n-2) d b a c
