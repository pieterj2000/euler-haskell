import Primes
import Digits
import qualified Data.Set as S


choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs


isconcatprime :: Int -> Int -> Bool
isconcatprime a b =
    let lena = numdigitswithlog a
        lenb = numdigitswithlog b
        ab = 10^lena * b + a
        ba = 10^lenb * a + b
    in isPrime ab && isPrime ba

allconcatprime :: [Int] -> Bool
allconcatprime [x] = True
allconcatprime (x:xs) = all (isconcatprime x) xs && allconcatprime xs

-- getnext :: [Int] -> [[Int]]
-- getnext xs = do
--     let maxX = head xs
--     y <- dropWhile (<= maxX) primes
--     [y:xs | all (isconcatprime y) xs]
--     --[y:xs]

-- getsetof :: Int -> [[Int]]
-- getsetof 1 = map (:[]) $ 3 : (drop 3 primes)    -- skip 2 en 5
-- getsetof n =
--     let setm1 = getsetof (n-1)
--         lijst = map getnext setm1
--         sift :: [[Int]] -> [[[Int]]] -> [[[Int]]]
--         sift y (x:xs)
--             | sum (head y) < sum (head x)   = y:x:xs
--             | otherwise                     = x : sift y xs

--         ordered :: [[[Int]]] -> [[Int]]
--         ordered ((x:xs):xss) = x : ordered (sift xs xss)

--     in ordered lijst

getnclique :: Int -> Int -> S.Set [Int]
getnclique 0 _ = S.empty
getnclique 1 mp = S.fromAscList $ map (:[]) $ takeWhile (<= mp) primes
getnclique 2 mp = S.fromList [ [a,b] | a<-(takeWhile (<= mp) primes), b<-(takeWhile (<= mp) $ dropWhile (<=a) primes), isconcatprime a b]
getnclique n mp = 
    let 


    in undefined

insertList :: Ord a => [a] -> S.Set a -> S.Set a
insertList list set = foldr S.insert set list

getcliques :: [(S.Set [Int], S.Set [Int], S.Set [Int])]
getcliques = scanl doe (S.empty, S.empty, S.empty) (tail primes)
    where
        doe :: (S.Set [Int], S.Set [Int], S.Set [Int]) -> Int -> (S.Set [Int], S.Set [Int], S.Set [Int])
        doe (set3clique, set4clique, set5clique) p =
            let neighbors = filter (isconcatprime p) $ takeWhile (<p) primes
                cliques2 = filter (\[a,b] -> isconcatprime a b) $ choose 2 neighbors
                set3clique' = insertList (map (++ [p]) cliques2) set3clique
                cliques3 = filter (`S.member` set3clique) $ choose 3 neighbors
                set4clique' = insertList (map (++ [p]) cliques3) set4clique
                cliques4 = filter (`S.member` set4clique) $ choose 4 neighbors
                set5clique' = insertList (map (++ [p]) cliques4) set5clique
            in (set3clique', set4clique', set5clique')

get5cliques = dropWhile S.null $ map (\(a,b,c) -> c) getcliques


main = do
    let c = head get5cliques
        c' = head $ S.elems c
    print c'
    print $ sum c'

    return ()