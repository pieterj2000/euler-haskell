

-- continued fraction van sqrt n
fraction :: Int -> [Int]
fraction n =
    let 
        doe :: [Int] -> [(Int, Int, Int)] -> [(Int,(Int, Int, Int))]
        doe as rs = 
            let (alpha, beta, gamma) = head rs
                anext :: Int
                anext = let a = fromIntegral alpha 
                            b = fromIntegral beta
                            c = fromIntegral gamma
                            k = fromIntegral n
                        in floor $ c / (a*(sqrt k) + b)
                alphanext = alpha*gamma
                betanext = (-alpha)*alpha*n*anext - beta*gamma + beta*beta*anext
                gammanext = alpha*alpha*n - beta*beta
                d = gcd (gcd alphanext betanext) gammanext
                rnext = (alphanext `div` d, betanext `div` d, gammanext `div` d)

                as' = (anext:as)
                rs' = (rnext:rs)
            in if rnext `elem` rs
                then zip as' rs'
                else doe as' rs'

        a = floor $ sqrt $ fromIntegral n
        r = (1,-a,1)

        (as,rs) = unzip $ doe [a] [r]
    in reverse as

fractioncycle = tail . fraction

cyclelength = length . fractioncycle

issquare :: Int -> Bool
issquare n = n == (floor $ sqrt $ fromIntegral n)^2

result = length $ filter odd $ map cyclelength $ filter (not . issquare) [2..10000] 


