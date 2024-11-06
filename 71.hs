import Data.Ratio
import Data.List (sort)


-- maxequal digits = let den = digits `div` 7 in (3*den, 7*den)

-- maxless digits = let (n,m) = maxequal digits in (n-1,m)

-- next digits (m,n) = (m+1,n+1)

-- --lijst digits = iterate (next digits) $ maxless digits

-- lijst digits (m,n)
--     | (m+1)*7 < (n+1)*3 && (n+1 <= digits)     = (m+1,n+1) : lijst digits (m+1,n+1)
--     | (m+1)*7 < 3*n         = (m+1,n) : lijst digits (m+1,n)
--     | n+1 > digits          = []
--     | (m+1)*7 > (n+1)*3     = []

-- isgoed digits (m,n) = (n <= digits) && (7*m < 3*n)

-- --lijstgoed digits = takeWhile (isgoed digits) $ lijst digits

-- lijstgoed digits = lijst digits $ maxless digits


bruteforce = maximum $ filter (< 3 % 7) $ map (\n -> ((3 * n) `div` 7) % n ) [8..1000000]

main = print bruteforce


nextfarey (a,b) = (a+3,b+7)


nextfarey' :: (Int, [(Int,Int)]) -> (Int, [(Int,Int)])
nextfarey' (n, [x]) = (n+1, [x])
nextfarey' (n, (x:y:xs)) =
    let (_, nlist) = nextfarey' (n, y:xs)
        (a,b) = x
        (c,d) = y
    in (n+1, if b + d <= n+1 then (a,b) : (a+c, b+d) : nlist else (a,b): nlist )

lijst = iterate nextfarey (2,5)
lijst' = iterate nextfarey' (2,[(0,1),(1,2),(1,1)])



antwoord = last $ takeWhile ((<= 1000000) . snd) lijst
