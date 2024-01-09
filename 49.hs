import Primes
import Digits
import Data.List (sort)

main = print calc

calc = concatMap werktPriem relevantPrimes

ub, lb :: Int
ub = 9876
lb = 1234

relevantPrimes :: [Int]
relevantPrimes = takeWhile (<ub) $ dropWhile (<lb) $ primes

werktPriem :: Int -> [(Int, Int, Int)]
werktPriem n = 
    let speling = ub - n
        mogTweede = takeWhile (< n + (speling `div` 2)) $ dropWhile (<=n) relevantPrimes
        derde t = n + 2*(t-n)
        derdePriem = isPrime . derde
        isPerm x = (sort $ intToDigitsRev x) == (sort $ intToDigitsRev n)
        isGoeie t = isPerm t && derdePriem t && (isPerm $ derde t)
    in map (\t -> (n,t,derde t)) $ filter isGoeie mogTweede