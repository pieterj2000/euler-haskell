import Primes
import Digits

main = print calc

calc = (`mod` (10^10)) .sum . map term $ [1..1000]
calc' = (`mod` (10^10)) .sum . map (\x -> x^x) $ [1..1000]


term :: Integer -> Integer
term n = expmod n n (10^10)


-- a^m mod n
--expmod :: Int -> Int -> Int -> Int
expmod :: Integral a => a -> a -> a -> a
expmod a m n = 
    let binaryPowersModulo = iterate ((`mod` n) . (^2)) (a `mod` n)
        binRep = intToDigitsRevBase 2 m
        list = zipWith (^) binaryPowersModulo binRep
    in  foldr1 (((`mod` n) . ) . (*)) list