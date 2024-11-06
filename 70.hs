import Primes
import Digits
import Data.List (sort, sortBy, sortOn, maximumBy)
import Data.Ord (comparing, Down (Down))
import Data.Function (on)


rprimes = reverse $ takeWhile (\p -> p < (10^7 `div` 2)) primes

possiblepowers :: Int -> [(Int,Int)]
possiblepowers p = reverse $ takeWhile (\(p,k) -> p^k < 10^7) $ map (\k -> (p,k)) [2..]

possiblepowers2 :: Int -> Int -> [([(Int, Int)],Int)]
possiblepowers2 p q =
    let p1l = iterate (\(n,m,v) -> (n+1,m,v*p)) (1,0,p)
        p2l = iterate (map (\(n,m,v) -> (n,m+1,v*q))) p1l
        p2l' = map (takeWhile (\(_,_,v) -> v<10^7)) p2l
        pl = concat $ takeWhile (not . null) p2l'
        pls = sortOn (Down . (\(_,_,v) -> v)) pl
        pls' = map (\(n,m,v) -> ( filter (\(_,s) -> s > 0) [(p,n),(q,m)],v)) pls
    in init pls'



isgoed pks =
    let t = eulerTotientFromPrimeFactorization pks
        td = intToDigitsRev t
        p = fromFactorizationMult pks
        pd = intToDigitsRev p
    in sort td == sort pd


test1 = filter isgoed $ concatMap (map (:[]) .  possiblepowers) rprimes
test2metpowers =
    let l = concat [possiblepowers2 p q | p<-rprimes, q<-(reverse $ takeWhile (\s -> s*p < 10^7) primes)]
        l' = filter (isgoed . fst) l
    in maximumBy (compare `on` (\(fs,v) -> fromIntegral (eulerTotientFromPrimeFactorization fs) / (fromIntegral v :: Double))) l'
test2 =
    let l = concat [ [([(p,1),(q,1)],p*q)] | p<-rprimes, q<-(reverse $ takeWhile (\s -> s*p < 10^7) primes)]
        l' = filter (isgoed . fst) l
    in maximumBy (compare `on` (\(fs,v) -> fromIntegral (eulerTotientFromPrimeFactorization fs) / (fromIntegral v :: Double))) l'


everynth :: Int -> [a] -> [a]
everynth _ [] = []
everynth n (x:xs) = x : everynth n (drop n xs)
main = print (head rprimes) >> print test2