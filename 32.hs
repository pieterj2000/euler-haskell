import Data.List (nub, sort)


main = print calc

--calc :: Int
calc = sum . 
    nub . 
    sort .
    map (\(_,_,a) -> a) .
    filter isPandigital $ possibilities

--toDigit :: [Int] -> Int
--toDigit [a,b,c,d] = 1000*a+100*b+10*c+d

fromDigit :: Int -> [Int]
fromDigit n = [ n `quot` 1000, (n `mod` 1000) `quot` 100, (n `mod` 100) `quot` 10, n `mod` 10]

isPandigital :: ([Int],[Int],Int) -> Bool
isPandigital (a, b, c) = sort s == [1..9]
    where s = dropWhile (==0) a ++ dropWhile (==0) b ++ dropWhile (==0) (fromDigit c)

mult :: [Int] -> [Int] -> Int
mult [a,b] [c,d,e,f] = (10*a + b)*(1000*c+100*d+10*e+f)

possibilities :: [([Int], [Int], Int)]
possibilities = [ ([a,b], [c,d,e,f], mult [a,b] [c,d,e,f]) |
                    a<-[0..9],
                    b<-0:filter (/=a) [1..9],
                    c<-0:filter (`notElem` [a,b]) [1..9],
                    d<-0:filter (`notElem` [a,b,c]) [1..9],
                    e<-0:filter (`notElem` [a,b,c,d]) [1..9],
                    f<-0:filter (`notElem` [a,b,c,d,e]) [1..9],
                    mult [a,b] [c,d,e,f] < 10000
                ]

