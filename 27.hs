import Primes
import Data.List (maximumBy, nub)
import Data.Ord (comparing)


main = print calc

calc = uncurry (*) . fst . maximumBy (comparing snd) $
    [ ((p-b-1,b), numPrimes (p-b-1) b) | b<-takeWhile (<= 1000) primes, p<-takeWhile (<= 2000) primes, (p-b-1) <= 1000 ]

numPrimes :: Int -> Int -> Int
numPrimes a b = length . takeWhile isPrime . map f $ [2..]
    where f n = n*n+a*n+b