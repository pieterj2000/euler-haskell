import Primes

main = print calc

calc = head $ filter (not . isGoldBach) $ filter odd $ filter (not . isPrime) [3..]

isGoldBach :: Int -> Bool
isGoldBach n = any (isDoubleSquare . (n-)) $ possiblePrimes n

possiblePrimes :: Int -> [Int]
possiblePrimes n = takeWhile (<n) primes

isDoubleSquare :: Int -> Bool
isDoubleSquare = isSquare . (`div` 2)

isSquare :: Int -> Bool
isSquare = isInt . sqrt . fromIntegral

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)