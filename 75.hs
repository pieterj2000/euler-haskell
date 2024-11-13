import Primes
import Control.Monad (guard)
import Data.List (sortOn, (\\))

primitives :: [(Int,Int,Int)]
primitives = do
    m<-[2..10]
    n<-[1..(m-1)]
    if even m
        then guard (odd n)
        else guard (even n)
    let circum = 2*m*(m+n)
        maxk = 300 `div` circum
    guard (circum <= 300)
    let a = m*m-n*n
        b = 2*m*n
        c = m*m+n*n
    return (min a b, max a b, c)


triples :: [(Int,Int,Int,Int)]
triples = do
    let maxcircum = 1_500_000
        possms = takeWhile (\m -> 2*m*(m+1) <= maxcircum) [2..]
    m<-possms
    n<-[1..(m-1)]
    let circum = 2*m*(m+n)
        maxk = maxcircum `div` circum
    if even m
        then guard (odd n)
        else guard (even n)
    k <- [1..maxk]
    let a = k*(m*m-n*n)
        b = k*2*m*n
        c = k*(m*m+n*n)
    return (min a b, max a b, c, k*circum)

triples' :: [(Int,Int,Int,Int)]
triples' = do
    let maxcircum = 1_500_000
        possms = takeWhile (\m -> 2*m*(m+1) <= maxcircum) [2..]
    m<-possms
    n<-[1..(m-1)]
    let circum = 2*m*(m+n)
        maxk = maxcircum `div` circum
    guard (even n && even m)
    k <- [1..maxk]
    let a = k*(m*m-n*n)
        b = k*2*m*n
        c = k*(m*m+n*n)
    return (min a b, max a b, c, k*circum)

sortt :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
sortt = sortOn (\(a,b,c,d) -> (d,a,b,c))

--more :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
more :: Eq b => [b] -> [(b, b)]
more xs = filter (uncurry (==)) $ zip xs (tail xs)

nubsorted :: Eq a => [a] -> [a]
nubsorted [] = []
nubsorted [x] = [x]
nubsorted (x:y:xs)
    | x == y = nubsorted (y:xs)
    | otherwise = x : nubsorted (y:xs)

spul = more $ map (\(a,b,c,d) -> d) $ sortt triples


alleenuniek :: Eq a => [a] -> [a]
alleenuniek [] = []
alleenuniek [x] = [x]
alleenuniek (x:y:xs) 
    | x == y = alleenuniek (dropWhile (==x) xs)
    | otherwise = x : alleenuniek (y:xs)


foutresultaat = length $ nubsorted spul
resultaat = length $ alleenuniek $ map (\(a,b,c,d) -> d) $ alleenuniek $ nubsorted $ sortt triples

--main = print $ length $ nubsorted $ map (\(a,b,c,d) -> d) $ sortt triples
-- main = print $ length $ nubsorted $ sortt triples
--main = print $ triples' \\ triples
-- main = print $ length $ nubsorted $ sortt triples
