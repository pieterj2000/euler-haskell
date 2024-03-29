module Digits (
intToDigitsRev,
intToDigits,
digitsToInt,
intToDigitsRevBase,
digitsToIntBase,
digitsToIntRevBase,
digitsToIntRev
) where

    
intToDigitsRev :: Int -> [Int]
intToDigitsRev = intToDigitsRevBase 10

-- TODO: kijken of niet beter werkt met isLeftTruncatablePrime en numDigits zoals in 37.hs
intToDigits :: Int -> [Int]
intToDigits = reverse . intToDigitsRev

digitsToInt :: [Int] -> Int
digitsToInt = digitsToIntBase 10
    
--intToDigitsRevBase :: Int -> Int -> [Int]
intToDigitsRevBase :: Integral t => t -> t -> [t]
intToDigitsRevBase _ 0 = []
intToDigitsRevBase b n = (n `mod` b) : intToDigitsRevBase b (n `div` b)

digitsToIntBase :: Int -> [Int] -> Int
digitsToIntBase b = foldl1 (\acc el -> acc * b + el)

digitsToIntRevBase :: Int -> [Int] -> Int
digitsToIntRevBase b [] = 0
digitsToIntRevBase b (x:xs) = x + b*(digitsToIntRevBase b xs)

digitsToIntRev :: [Int] -> Int
digitsToIntRev = digitsToIntRevBase 10