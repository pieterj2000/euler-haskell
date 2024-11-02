

lb :: Int -> Int
lb n = ceiling $ 10 ** (1-1/(fromIntegral n))

nums :: [Int]
nums = takeWhile (<10) $ map lb [1..]

pernum :: [Int]
pernum = map (10 -) nums

totaal = sum pernum