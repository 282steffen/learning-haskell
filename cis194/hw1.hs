{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module HomeWork1 where

-- HW1
-- Excercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev a = if a <= 0 
                then [] 
                else (a `mod` 10) : toDigitsRev (a `quot` 10)

toDigits :: Integer -> [Integer]
toDigits a = if a <= 0
             then []
             else toDigits (a `quot` 10) ++  (a `mod` 10) : []

--Excercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = if null xs
                        then []
                        else if (length (tail xs) `mod` 2) == 0 
                                then head xs : doubleEveryOther (tail xs)
                                else ((head xs) * 2) : doubleEveryOther (tail xs)

--Excercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = if null xs then 0 else 
                                    if (head xs) > 9
                                        then (head xs) `div` 10 + (head xs) `mod` 10 + sumDigits (tail xs)
                                        else  (head xs) + sumDigits (tail xs)

--Exercise 4
validate :: Integer -> Bool
validate a 
    | a > 0 = (sumDigits (doubleEveryOther (toDigits a))) `mod` 10 == 0
    | otherwise = False

--Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi i a b c
    | i <= 0 = []
    | otherwise = (hanoi (i - 1) a c b) ++ [(a,b)] ++ (hanoi (i - 1) c b a) 

