import Digits
import Primes (divisor)


main = print calc

--calc :: Int
calc = sum $ filter (isPalindromeBase 2) $ filter (isPalindromeBase 10) $ possibilities

isPalindromeBase :: Int -> Int -> Bool
isPalindromeBase b n = let ds = intToDigitsRevBase b n in ds == reverse ds

-- kan véél beter door ze expliciet te construeren, maar geen zin in gehad
possibilities :: [Int]
possibilities = filter odd [1..999999]
