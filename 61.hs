import Data.List (permutations)

p3solven,p4solven,p5solven,p6solven,p7solven,p8solven :: Double -> Double
p3solven x = (sqrt (8*x+1) - 1) / 2
p4solven = sqrt
p5solven x = (sqrt (24*x+1) + 1) / 6
p6solven x = (sqrt (8*x+1) + 1) / 4
p7solven x = (sqrt (40*x+9) + 3) / 10
p8solven x = (sqrt (3*x+1) + 1) / 3

p3,p4,p5,p6,p7,p8 :: Int -> Int
p3 n = n*(n+1) `div` 2
p4 n = n*n
p5 n = n*(3*n-1) `div` 2
p6 n = n*(2*n-1)
p7 n = n*(5*n-3) `div` 2
p8 n = n*(3*n-2)


getPossible :: Int -> ((Double -> Double), (Int -> Int)) -> [Int]
getPossible n (finv, f) =
    let digits = n `mod` 100
        xlb = 100*digits
        xub = 100*digits + 99
        nlb = ceiling $ finv $ fromIntegral xlb
        nub = floor $ finv $ fromIntegral xub
        possible = map f [nlb..nub]
        goeie = filter (\n -> (n `mod` 100) >= 10) possible
    in goeie

chain :: [((Double -> Double), (Int -> Int))] -> [[Int]] -> [[Int]]
chain [] lists = map (reverse . tail) $ filter (\l -> head l == last l) lists
chain (f:fs) lists = 
    let nexts = do
            l <- lists
            let target = head l
                opts = getPossible target f
            map (:l) opts
    in chain fs nexts

fs = [(p3solven, p3), (p4solven, p4), (p5solven, p5), (p6solven, p6), (p7solven, p7), (p8solven, p8)]

chainperms :: [((Double -> Double), (Int -> Int))] -> [[Int]] -> [[Int]]
chainperms fs ls = concatMap (\fp -> chain fp ls) $ permutations fs

findchain :: [((Double -> Double), (Int -> Int))] -> [[Int]]
findchain fs = head $ filter (not . null) $ map (\n -> chainperms fs [[n]]) [1000..9999]

result = findchain fs
result' = sum $ head $ result

main = do
    let r = result
    print r
    print $ sum $ head r