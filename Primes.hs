module Primes (
    primes,
    divisor,
    primeFactorization,
    isPrime,
    isPrime',
    isPrime''
) where
    
    divisor :: Int -> Int -> Bool
    divisor a b = b `mod` a == 0

    primes :: [Int]
    primes = 2:3:[n | n<-[5,7..], isPrime n]

    primeFactorization :: Int -> [Int]
    primeFactorization 1 = []
    primeFactorization x = d : primeFactorization (x `div` d)
        where
            d = head . filter (`divisor` x) $ primes

    isPrimeWithDivisorList :: [Int] -> Int -> Bool
    isPrimeWithDivisorList _ n | n <= 1 = False
    isPrimeWithDivisorList xs n = not . any (`divisor` n) . takeWhile (<= (round . sqrt . fromIntegral $ n) ) $ xs

    -- | Checks if every number in primes <= sqrt n is a divisor, has to generate primes up to sqrt n if not done yet.
    --   Most of the time fastest
    isPrime :: Int -> Bool
    isPrime = isPrimeWithDivisorList primes

    -- | Checks if n is in primes, has to generate up to n if not done yet
    --   Probably slowest, unless
    isPrime' :: Int -> Bool
    isPrime' n = elem n $ takeWhile (<=n) primes

    -- | Most naive, check every number in [2..sqrt n] if divisor
    isPrime'' :: Int -> Bool
    isPrime'' = isPrimeWithDivisorList [2..]


    ------------------OUD----------------------------------------------------

        
    divisorMult :: Int -> Int -> Int
    divisorMult a b
        | not $ divisor a b = 0
        | otherwise = 1 + divisorMult a (b `div` a)
    
    primeFactorizationMult :: Int -> [(Int, Int)]
    primeFactorizationMult 1 = []
    primeFactorizationMult x = (d,m) : primeFactorizationMult (x `div` d^m)
        where
            d = head . filter (`divisor` x) $ primes --[2..x]
            m = divisorMult d x
            
    fromFactorizationMult :: [(Int, Int)] -> Int
    fromFactorizationMult = foldr (\(p, n) acc -> acc * p^n) 1
            
    eulerTotientFromPrimeFactorization :: [(Int, Int)] -> Int
    eulerTotientFromPrimeFactorization [] = 1
    eulerTotientFromPrimeFactorization ((p, n):xs) = p^(n-1) * (p-1) * eulerTotientFromPrimeFactorization xs
    
    eulerTotient :: Int -> Int
    eulerTotient = eulerTotientFromPrimeFactorization . primeFactorizationMult

    -- returns binary representation in list of digits from least to most significant
    binaryRepr :: Int -> [Int]
    binaryRepr 0 = []
    binaryRepr n
        | even n = 0 : binaryRepr (n `div` 2)
        | odd n = 1 : binaryRepr (n `div` 2)

    -- a^m mod n
    expmod :: Int -> Int -> Int -> Int
    expmod a m n = 
        let binaryPowersModulo = iterate ((`mod` n) . (^2)) (a `mod` n)
            binRep = binaryRepr m
            list = zipWith (^) binaryPowersModulo binRep
        in  foldr1 (((`mod` n) . ) . (*)) list
    
    -- multiplicative order of a mod n with third argument phi(n) and fourth argument its factorization
    orderTotient :: Int -> Int -> Int -> [(Int, Int)] -> Int
    orderTotient a n totient [] = totient
    orderTotient a n totient ((p, pn):xs)
        | divises = orderTotient a n newtotient (if pn == 1 then xs else (p, pn-1):xs)
        | otherwise = orderTotient a n totient xs
            where 
                newtotient = totient `div` p 
                divises = expmod 10 newtotient n == 1

    -- calculates multiplicative order of a mod n with third paramter being factorization of n
    order :: Int -> Int -> [(Int, Int)] -> Int
    order a n nfact  = orderTotient a n totient totientfact
        where 
            totient = eulerTotientFromPrimeFactorization nfact
            totientfact = primeFactorizationMult totient

         
