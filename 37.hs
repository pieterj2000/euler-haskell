import Primes

main = print calc

--calc :: Int
calc = sum $ filter isLeftTruncatablePrime rightTruncatablePrimes

rightTruncatablePrimes :: [Int]
rightTruncatablePrimes = filter (> 9) . concat . takeWhile (not . null) . iterate (>>= addDigit) . filter isPrime $ [1..9]
    where   addDigit :: Int -> [Int]
            addDigit n = filter isPrime $ map (10*n+) [1,3,7,9]

numDigits :: Int -> Int
numDigits n = if n < 10 then 1 else 1 + numDigits (n `div` 10)

isLeftTruncatablePrime :: Int -> Bool
isLeftTruncatablePrime n = all (isPrime . (n `mod`) . (10^)) [1..numDigits n]