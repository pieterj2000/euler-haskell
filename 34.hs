

main = print calc

calc :: Int
calc = sum . filter (\n -> sumFacts n == n) $ [3..1000000]

intToDigits :: Int -> [Int]
intToDigits 0 = []
intToDigits n = (n `mod` 10) : intToDigits (n `div` 10)

sumFacts :: Int -> Int
sumFacts = sum . map fact . intToDigits

fact :: Int -> Int
fact n = product [1..n]

-- maximum is 7 digits, as 7*9! ~ 2600000