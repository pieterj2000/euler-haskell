import Primes
import Data.List (permutations, foldl', sortOn, nub)

import Debug.Trace

main = print calc

calc = findConsecutives'


withFactors :: [Int] -> [Int]
withFactors [] = error "geen factors"
withFactors (f:fs) =
    let firstFactor = map (f^) [0..]
    in foldl' extendWithFactor firstFactor fs

withFactorsNoPrimes :: [Int] -> [Int]
withFactorsNoPrimes [] = error "geen factors"
withFactorsNoPrimes (f:fs) =
    let firstFactor = map (f^) [1..]
    in foldl' extendWithFactorNoPrimes firstFactor fs

extendWithFactorNoPrimes :: [Int] -> Int -> [Int]
extendWithFactorNoPrimes xs p =
    let firstcol = map (p^) [1..]
        makecol x = map (*x) firstcol
        arr = map makecol xs
        diags = getDiags arr
    in combineListsOrdered diags

extendWithFactor :: [Int] -> Int -> [Int]
extendWithFactor xs p =
    let firstcol = map (p^) [0..]
        makecol x = map (*x) firstcol
        arr = map makecol xs
        diags = getDiags arr
    in combineListsOrdered diags

getDiags :: [[Int]] -> [[Int]]
getDiags xss =
    let firstcol = map head xss
        next = [] : getDiags (map tail xss)
    in zipWith (:) firstcol next


calcFromPrimePowers :: [Int] -> [Int] -> Int
calcFromPrimePowers ps = product . zipWith (^) ps

-- iedere sublist is geordered
-- head van alle sublists zijn geordered
combineListsOrdered :: (Ord a) => [[a]] -> [a]
combineListsOrdered ([]:rest) = combineListsOrdered rest
combineListsOrdered (as:[]) = as
combineListsOrdered ((a:as):bs:rest)
    | as == []          = a : next (bs:rest)
    | otherwise         = a : next (as:bs:rest)
        where
            next = combineListsOrdered . swaptop2ifneccesary
            swaptop2ifneccesary :: (Ord a) => [[a]] -> [[a]]
            swaptop2ifneccesary [as] = [as]
            swaptop2ifneccesary (as:bs:rest)
                | as == []          = swaptop2ifneccesary (bs:rest)
                | bs == []          = swaptop2ifneccesary (as:rest)
                | head as > head bs = bs : swaptop2ifneccesary (as:rest)
                | otherwise         = as:bs:rest


chooseInclUpto :: [Int] -> Int -> Int -> [[Int]]
chooseInclUpto ys n xMax =
    let
        go :: [Int] -> Int -> [[Int]]
        go xs 1 = [[xMax]]
        go [] n = []
        go (x:xs) n = (map (x:) $ go xs (n-1)) ++ (go xs n)
    in go (takeWhile (< xMax) ys) n

chooseInclSortedUpto :: [Int] -> Int -> Int -> [[Int]]
chooseInclSortedUpto ys n xMax = sortOn product $ chooseInclUpto ys n xMax

combos4primesUpto :: Int -> [[Int]]
combos4primesUpto = chooseInclSortedUpto primes 4

numbers4primesPerFactorsUpto :: Int -> [[Int]]
numbers4primesPerFactorsUpto = map withFactorsNoPrimes . combos4primesUpto

numbers4primesUpto :: Int -> [Int]
numbers4primesUpto = combineListsOrdered . numbers4primesPerFactorsUpto

numbers4primes :: [Int]
numbers4primes = combineListsOrdered $ map numbers4primesUpto $ drop 3 primes

findConsecutives :: Int
findConsecutives =
    let go :: [Int] -> Int
        go (a:b:c:d:rest)
            | (a+1) == b && (b+1) == c && (c+1) == d = a
            | otherwise = go (b:c:d:rest)

    in go numbers4primes

findConsecutives' :: Int
findConsecutives' = head $ filter consecutiveGood [2..]

consecutiveGood :: Int -> Bool
consecutiveGood = all has4primefactors . take 4 . iterate (+1)

has4primefactors :: Int -> Bool
has4primefactors n = length (nub $ primeFactorization n) == 4

--orderedUnion :: Ord a => [[a]] -> [a]
--orderedUnion = orderedUnionBy id
--
--orderedUnionBy :: Ord b => (a -> b) -> [[a]] -> [a]
--orderedUnionBy f = foldr1 (orderedUnion2By f)
--
--orderedUnion2 :: Ord a => [a] -> [a] -> [a]
--orderedUnion2 = orderedUnion2By id
--
--orderedUnion2By :: Ord b => (a -> b) -> [a] -> [a] -> [a]
--orderedUnion2By f (a:as) (b:bs)
--    | f a < f b = a : orderedUnion2By f as (b:bs)
--    | otherwise = b : orderedUnion2By f (a:as) bs
--