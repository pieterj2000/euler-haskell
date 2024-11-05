import Data.List (nub, permutations, sortBy, minimumBy)
import Data.Function (on)
import Data.Char (isDigit)

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs


isgoed :: Int -> [Int] -> Bool
isgoed som xs = positief && length (nub $ uitsteeksels ++ xs) == 2 * (length xs)
    where
        uitsteeksels = zipWith (\i j -> som - i - j) xs (tail $ cycle xs)
        positief = all (\x -> x>0 && x <= 2*(length xs)) uitsteeksels


uitsteeksels som xs = zipWith (\i j -> som - i - j) xs (tail $ cycle xs)
uitsteektupels xs = let som = getsom xs in zipWith (\i j -> (som - i - j, i, j)) xs (tail $ cycle xs)

--test = filter (isgoed 10) $ choose 3 [1..6]
mogelijk5 = concat $ map (\n -> filter (\p -> isgoed n p && 10 `notElem` p) $ choose 5 [1..10] >>= permute) [14..18]
mogelijk3 = concat $ map (\n -> filter (isgoed n) $ choose 3 [1..6] >>= permute) [9..12]

getsom :: [Int] -> Int
getsom xs = let l = length xs; s = sum xs in (l*(2*l+1) + s) `div` l

maaknummer :: [Int] -> Int
maaknummer xs =
    let l = length xs
        som = getsom xs
        tuples = zipWith (\i j -> (som - i - j, i, j)) xs (tail $ cycle xs)
        draai (t:ts) = ts ++ [t]
        mintup' = minimumBy (compare `on` (\(a,_,_) -> a)) tuples
        getgoeie tups@(t:ts) = if t == mintup' then tups else getgoeie (draai tups)
        tuples' = getgoeie tuples
    in read $ filter isDigit $ show tuples'



permute :: [Int] -> [[Int]]
permute (x:xs) = map (x:) $ {-filter (\l -> head l < last l) $-} permutations xs

antwoord = maximum $ map maaknummer mogelijk5