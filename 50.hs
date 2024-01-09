import Primes
import Digits
import Data.List (unfoldr, tails)
import Debug.Trace

main = print calc

calc = getFirst $ getDiags $ map mogelijkheden [0..]

limiet = 1000000 :: Int


startSpul :: [Int] -> [Int]
startSpul ls = unfoldr (\(xs,acc) -> let x = head xs in if not (null xs) && acc + x < limiet then Just (x, (tail xs, acc+x)) else Nothing) (ls,0)

startendBij :: Int -> [Int]
startendBij n = reverse $ startSpul primeslijst
    where
        primeslijst = drop n primes

mogelijkheden :: Int -> [[Int]]
mogelijkheden n = init $ tails $ startendBij n

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal ([]:_) = []
diagonal ((a:as):ass) = a : diagonal (map tail ass)

getDiags :: [[a]] -> [[a]]
getDiags xss =
    let firstcol = map head xss
        next = [] : getDiags (map tail xss)
    in zipWith (:) firstcol next

getFirst :: [[[Int]]] -> (Int, [Int])
getFirst xs
    | null goed = getFirst $ tail xs
    | otherwise = head goed
    where
        diag = map (\lijst -> (sum lijst, lijst)) $ head xs
        goed = filter (isPrime . fst) diag



-- maxStartingFromNthPrime :: Int -> (Int, [Int])
-- maxStartingFromNthPrime n = (sum r, r)
--     where
--         primeslijst = drop n primes
--         lijstjesPriem = tail $ inits primeslijst
--         lijstjes = map startSpul lijstjesPriem
--         lijstjesgoed = filter (isPrime . sum) lijstjes
--         r = head lijstjesgoed
-- 
-- 
-- startSpul :: ([Int],[Int])
-- startSpul = (links, rechts)
--     where
--         links = unfoldr (\(x:xs,acc) -> if acc + x < limiet then Just (x, (xs, acc+x)) else Nothing) (primes,0)
--         rechts = drop (length links) primes

-- maxmogelijkheden :: [(Int, [Int])]
-- maxmogelijkheden =
--     let go :: ([Int],[Int]) -> Int -> [(Int, [Int])]
--         go  (wel, niet) som
--             | som == 0    = []
--             | null wel          = []
--             | canAppend         = (som, wel) : go (appended, tail niet) appendedSom
--             | otherwise         = (som, wel) : go (anders, niet) andersSom
--             where
--                 appended = tail wel ++ [head niet]
--                 appendedSom = som - (head wel) + (head niet)
--                 canAppend = appendedSom < limiet
--                 anders = tail wel
--                 andersSom = som - (head wel)

--         start = startSpul
--         startSom = sum $ fst start

--         -- go a b = traceShow (fst a,b) $ go' a b 
--     in go start startSom