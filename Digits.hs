module Digits (
intToDigitsRev,
intToDigits,
digitsToInt,
intToDigitsRevBase,
digitsToIntBase,
digitsToIntRevBase,
digitsToIntRev,
numdigits,
numdigitswithlog
) where
import Data.List (foldl1')

    
intToDigitsRev :: Integral a => a -> [a]
intToDigitsRev = intToDigitsRevBase 10

-- TODO: kijken of niet beter werkt met isLeftTruncatablePrime en numDigits zoals in 37.hs
intToDigits :: Int -> [Int]
intToDigits = reverse . intToDigitsRev

digitsToInt :: Integral a => [a] -> a
digitsToInt = digitsToIntBase 10
    
--intToDigitsRevBase :: Int -> Int -> [Int]
intToDigitsRevBase :: Integral t => t -> t -> [t]
intToDigitsRevBase _ 0 = []
intToDigitsRevBase b n = (n `mod` b) : intToDigitsRevBase b (n `div` b)

digitsToIntBase :: Integral a => a -> [a] -> a
digitsToIntBase b = foldl1' (\acc el -> acc * b + el)

digitsToIntRevBase :: Int -> [Int] -> Int
digitsToIntRevBase b [] = 0
digitsToIntRevBase b (x:xs) = x + b*(digitsToIntRevBase b xs)

digitsToIntRev :: [Int] -> Int
digitsToIntRev = digitsToIntRevBase 10

numdigitswithlogBase :: Integral a => Int -> a -> Int
numdigitswithlogBase b = (+1) . floor . logBase (fromIntegral b) . fromIntegral

numdigitswithlog :: Integral a => a -> Int
numdigitswithlog = numdigitswithlogBase 10

numdigitsBase :: Integral a => a -> a -> Int
numdigitsBase b = length . intToDigitsRevBase b

numdigits :: Integral a => a -> Int
numdigits = length . intToDigitsRev