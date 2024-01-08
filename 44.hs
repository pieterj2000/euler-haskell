import Digits
import Control.Monad (guard)
import Data.List (sort)

main = print calc

calc = head $ filter possibleForDiff' $ pentagonalNumbers

possibleForDiff :: Int -> Bool
possibleForDiff diff =
    let mogelijkheden = [
                (kleine, grote) | --grote) |
                kleine <- takeWhile (<= (diff `div` 2)) pentagonalNumbers,
                let grote = diff + kleine,
                isPentagonal grote
            ]
        somPent (n,m) = isPentagonal (n+m)
    in any somPent mogelijkheden

possibleForDiff' :: Int -> Bool
possibleForDiff' diff =
    let kleine = takeWhile (<= (diff `div` 2)) pentagonalNumbers
        kan n = isPentagonal (diff + n) && isPentagonal (diff + n + n)
    in any kan kleine

pentagonalNumbers :: [Int]
pentagonalNumbers = map nthPentagonal [1..]

nthPentagonal :: Int -> Int
nthPentagonal n = (n*(3*n-1)) `div` 2

isPentagonal :: Int -> Bool
isPentagonal n = let d = sqrt $ 24*(fromIntegral n) + 1 in isInt d && (round d `mod` 6 == 5)

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)