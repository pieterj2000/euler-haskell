import Digits
import Data.List (sort)

main = print calc

calc = maximum $ map maxOfLength $ [1..4]

permutationsLexico :: Eq a => [a] -> [[a]]
permutationsLexico [] = [[]]
permutationsLexico xs = concatMap (\n -> map (n:) $ permutationsLexico (filter (/=n) xs) ) xs


permutationsChooseLexico :: Eq a => [a] -> Int -> [[a]]
permutationsChooseLexico xs n = map (take n) $ permutationsLexico xs

isPandigital :: [Int] -> Bool
isPandigital xs = sort xs == [1..9]

maxOfLength :: Int -> Int
maxOfLength n = (\xs -> if null xs then 0 else head xs) $ map digitsToInt $ filter isPandigital $ map doe $ permutationsChooseLexico [9,8..1] n

doe :: [Int] -> [Int]
doe x = let xInt = digitsToInt x
            poss = takeWhile ((<=9) . length . fst) $ dropWhile ((<9) . length . fst) $ 
                        iterate (\(y,i) -> (y ++ intToDigits ((i+1)*xInt), i+1) ) ([],0)
        in if null poss then [1,1] else fst (head poss)