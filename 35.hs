import Primes

main = print calc

--calc :: Int
calc = length . filter (all isPrime . cycles) $ possibilities

digitsToInt :: [Int] -> Int
digitsToInt = foldl1 (\acc el -> acc * 10 + el)

possibilities = 2:3:5:[ digitsToInt [a,b,c,d,e,f] |
                    a<-[0,1,3,7,9],
                    b<-(if a > 0 then tail else id) [0,1,3,7,9],
                    c<-(if b > 0 then tail else id) [0,1,3,7,9],
                    d<-(if c > 0 then tail else id) [0,1,3,7,9],
                    e<-(if d > 0 then tail else id) [0,1,3,7,9],
                    f<-(if e > 0 then tail else id) [0,1,3,7,9],
                    not $ 3 `divisor` (sum [a,b,c,d,e,f])
                ]

cycles :: Int -> [Int]
cycles x = let (n:ns) = iterate draai x in n : takeWhile (/=n) ns

draai :: Int -> Int
draai x = rest + coef * smallest
    where   coef = 10^(floor $ logBase 10 (fromIntegral x))
            (rest, smallest) = quotRem x 10