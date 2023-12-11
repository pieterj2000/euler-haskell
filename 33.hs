
main = print calc

--calc :: Int
calc = simplify $ mult $ concatMap (filter (\(a,b) -> a < b) . (\n -> abbc' n ++ abca' n)) [1..9]

abbc :: Int -> [(Int, Int, Int)]
abbc b = [ (a,b, 10*a*b `div` (9*a+b)) | a<-filter (/=b) [1..9], (9*a+b) `divides` (10*a*b) ]

abbc' :: Int -> [(Int, Int)]
abbc' = map (\(a,b,c) -> (10*a+b, 10*b+c)) . abbc

abca :: Int -> [(Int, Int, Int)]
abca a = [ (a, 10*a*c `div` (9*c+a),c) | c<-filter (/=a) [1..9], (9*c+a) `divides` (10*a*c) ]

abca' :: Int -> [(Int, Int)]
abca' = map (\(a,b,c) -> (10*a+b, 10*c+a)) . abca


divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

simplify :: (Int, Int) -> (Int, Int)
simplify (a,b) = let d = gcd a b in (a `div` d, b `div` d)

mult :: [(Int, Int)] -> (Int, Int)
mult [x] = x
mult ((a,b):(c,d):xs) = mult ((a*c,b*d):xs)