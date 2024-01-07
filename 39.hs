import Primes
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

main = print calc

calc = maximumBy (comparing numberTriples) feasiblep
--calc = possibleTriples 120

feasiblep :: [Int]
feasiblep = filter even [3..1000]

possibleTriples :: Int -> [(Int, Int, Int)]
possibleTriples p =
    let possiblebs = filter ((<p) . (*2)) [1..(1 + p `div` 2)]
        valid b = (2*(p-b)) `divides` (p*p)
        geta b = p - ( (p*p) `div` (2*(p-b)) )
        maketup b = (geta b, b, p - b - geta b)
    in map maketup $ filter valid possiblebs

numberTriples :: Int -> Int
numberTriples = (`div` 2) . length . possibleTriples

