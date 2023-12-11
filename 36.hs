import Digits
import Primes (divisor)


main = print calc

--calc :: Int
calc = sum $ filter (isPalindromeBase 2) $ filter (isPalindromeBase 10) $ possibilities

isPalindromeBase :: Int -> Int -> Bool
isPalindromeBase b n = let ds = intToDigitsRevBase b n in ds == reverse ds

possibilities :: [Int]
possibilities = filter (not . divisor 10) $ filter odd [1..999999]