module Digits (
intToDigitsRev,
digitsToInt,
intToDigitsRevBase,
digitsToIntBase
) where

    
intToDigitsRev :: Int -> [Int]
intToDigitsRev 0 = []
intToDigitsRev n = (n `mod` 10) : intToDigitsRev (n `div` 10)

digitsToInt :: [Int] -> Int
digitsToInt = foldl1 (\acc el -> acc * 10 + el)
    
intToDigitsRevBase :: Int -> Int -> [Int]
intToDigitsRevBase _ 0 = []
intToDigitsRevBase b n = (n `mod` b) : intToDigitsRevBase b (n `div` b)

digitsToIntBase :: Int -> [Int] -> Int
digitsToIntBase b = foldl1 (\acc el -> acc * b + el)
