import Primes
import Data.Ratio



corners :: Int -> [Int]
corners l = [ll-3*ld, ll-2*ld, ll-ld, ll]
    where
        ll = l*l
        ld = l-1

numprimes :: [Int]
numprimes = 0 : 0 : zipWith (\l prev -> prev + length (filter isPrime $ corners l)) [3..] numprimes

numprimesR :: [(Int,Double)]
numprimesR = zipWith (\l n -> (l, fromIntegral n / fromIntegral (2*l-1))) [1..] numprimes

--result = take 100 $ filter ( (< (1 % 10)) . numprimesR ) [3,5..]
result = 2 + (fst . last $ takeWhile ((>= 0.1) . snd) $ filter (odd . fst) $ drop 2 numprimesR)

main = print result