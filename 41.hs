import Digits
import Primes
import Data.List (sort)

main = print calc

calc = head $ concatMap pandigitalPrimes [7,6..1]

pandigitalPrimes :: Int -> [Int]
pandigitalPrimes n = filter isPrime $ map digitsToInt $ permutationsLexico [n,(n-1)..1]

permutationsLexico :: Eq a => [a] -> [[a]]
permutationsLexico [] = [[]]
permutationsLexico xs = concatMap (\n -> map (n:) $ permutationsLexico (filter (/=n) xs) ) xs