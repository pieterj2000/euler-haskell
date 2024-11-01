


factors :: Int -> [Int]
factors n = map round $ scanl1 (*) dingens
    where
        n' = fromIntegral n
        dingens = 1 : map ((\k -> (n'-k+1) / k) . fromIntegral) [1..n] :: [Double]

kleinerdanMiljoen :: Int -> Int
kleinerdanMiljoen = length . takeWhile (<= 1_000_000) . factors

groterdanMiljoen :: Int -> Int
groterdanMiljoen n = max (n + 1 - 2 * kleinerdanMiljoen n) 0

result = sum $ map groterdanMiljoen [1..100]