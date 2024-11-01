import Data.Char (isDigit, digitToInt)
import Data.List (sort, sortOn, group, sortBy)
import Data.Maybe (isJust)
import Control.Monad (forM_)
import qualified Data.Ord
import Data.Ord (comparing)
import Data.Char (digitToInt)
import Data.List (nub, sortBy)


main = do
    wordsInput <- readFile "54.poker.txt"
    let ls = lines wordsInput
    let games = map parseLine ls
    let results = map p1wins games

    --forM_ differents $ \(h1,h2) -> do
    --    print (h1, makeorders h1)
    --    print (h2, makeorders h2)
    --    print (evaluate h1 > evaluate h2)
    --print results
    print (length . filter id $ results)

    return ()

type Card = (Int, Char)
type Hand = [Card]

parseCard :: String -> Card
parseCard [n, k]
    | isDigit n = (digitToInt n, k)
    | n == 'T'  = (10, k)
    | n == 'J'  = (11, k)
    | n == 'Q'  = (12, k)
    | n == 'K'  = (13, k)
    | n == 'A'  = (14, k)
    | otherwise = error ([n,k])

parseLine :: String -> (Hand, Hand)
parseLine l = (map parseCard h1, map parseCard h2)
    where
        cs = words l
        h1 = take 5 cs
        h2 = drop 5 cs

p1wins :: (Hand,Hand) -> Bool
p1wins (h1,h2) = makeorders h1 > makeorders h2

makeorders :: Hand -> ([Int],[Int])
makeorders h =
    let h' = sort $ map fst h
        groeped = group h'
        counts = map length groeped
        cards = map head groeped
        (counts', cards') = sorttwolists counts cards

        counts''
            | royalflush h = [5,2]
            | straightflush h = [5,1]
            | flush h = [3,1,3]
            | straight h = [3,1,2]
            | otherwise = counts'

    in (counts'', cards')


sorttwolists :: Ord a => [a] -> [a] -> ([a],[a])
sorttwolists l1 = unzip . sortBy (comparing Data.Ord.Down) . zip l1


--p1wins :: (Hand,Hand) -> Bool
--p1wins (h1,h2) = doe (resultaten h1) (resultaten h2)
--    where
--        doe [] _ = False
--        doe _ [] = True
--        doe (x:xs) (y:ys) = case compare x y of
--            GT -> True
--            LT -> False
--            EQ -> case x of 
--                Nothing -> doe xs ys
--                Just i -> case compare (highcardAlsalis i h1) (highcardAlsalis i h2) of
--                    GT -> True
--                    LT -> False
--                    EQ -> doe highcardAlsalis i (filterRemoveOnce ((/=i) . fst) h1) (ys ++ [highcardAlsalis i (filterRemoveOnce ((/=i) . fst) h2)]) --error (show (h1,h2,i))




resultaten :: Hand -> [Maybe Int]
resultaten h = map ($ h) [royalflush', straightflush', fourofkind, fullhouse, flush', straight', threeofkind, twopairs, pair, highcard]


highcard :: Hand -> Maybe Int
highcard h = if null h then Nothing else Just $ maximum $ map fst h


filterRemoveOnce:: (a -> Bool) -> [a] -> [a]
filterRemoveOnce _ [] = []
filterRemoveOnce f (x:xs) = if f x then filterRemoveOnce f xs else xs

highcardAlsalis :: Int -> Hand -> Maybe Int
highcardAlsalis x h = highcard rest
    where
        rest = filterRemoveOnce ((/=x) . fst) h
        --rest = filter ((`notElem` algeweest) . fst) h
        --algeweest = if x <= 15 then [x] else (\(a,b) -> [a,b]) $ quotRem x 15


fullhouse :: Hand -> Maybe Int -- als twee three of kind, wat is dan significanter? Of is het ook gewoon high card
fullhouse h = case threeofkind h of
    Nothing -> Nothing
    Just x  ->
        let rest = filter ((/= x) . fst) h
        in (\y -> x * 15 + y) <$> pair rest

twopairs :: Hand -> Maybe Int -- als twee pair, wat is dan significanter? Of is het ook gewoon high card
twopairs h = case pair h of
    Nothing -> Nothing
    Just x  ->
        let rest = filter ((/= x) . fst) h
        in (\y -> (max x y) * 15 + (min x y)) <$> twoofkind rest

pair :: Hand -> Maybe Int
pair = twoofkind

allsame :: Eq a => [a] -> Bool
allsame [] = True
allsame [a] = True
allsame (x:xs) = all (==x) xs


nofkind :: Int -> Hand -> Maybe Int
nofkind n h = if not $ null x then head x else Nothing
    where
        hsort = sort $ map fst h
        x = filter isJust $ zipWith (\a b -> if a == b then Just a else Nothing ) hsort $ drop (n-1) hsort

fourofkind :: Hand -> Maybe Int
fourofkind = nofkind 4

threeofkind :: Hand -> Maybe Int
threeofkind = nofkind 3

twoofkind :: Hand -> Maybe Int
twoofkind = nofkind 2


straight' :: Hand -> Maybe Int
-- straight h = if l - f == 4 then Just l else Nothing -- als beide flush hebben, geld dan de hoogste flush? Of gewoon highest card?
straight' h = if l - f == 4 then Just 10 else Nothing -- als beide flush hebben, geld dan de hoogste flush? Of gewoon highest card?
    where
        f = minimum . map fst $ h
        l = maximum . map fst $ h

straight :: Hand -> Bool
-- straight h = if l - f == 4 then Just l else Nothing -- als beide flush hebben, geld dan de hoogste flush? Of gewoon highest card?
straight h = l - f == 4 && length h == length (nub $ map fst h)
    where
        f = minimum . map fst $ h
        l = maximum . map fst $ h

straightflush' :: Hand -> Maybe Int
straightflush' h = if isJust (flush' h) then straight' h else Nothing

straightflush :: Hand -> Bool
straightflush h = flush h && straight h



royalflush' :: Hand -> Maybe Int
royalflush' h
    | isJust (flush' h) && sort (map fst h) == [10,11,12,13,14]   = Just 10
    | otherwise = Nothing

royalflush :: Hand -> Bool
royalflush h = flush h && sort (map fst h) == [10,11,12,13,14]

flush :: Hand -> Bool
flush h = let kleur = snd $ head h in all ((==kleur) . snd) h

flush' :: Hand -> Maybe Int
flush' h = let kleur = snd $ head h in if all ((==kleur) . snd) h then Just 10 else Nothing