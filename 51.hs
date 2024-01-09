import Digits
import Primes

main = print calc

calc = head $ filter is8fam $ dropWhile (<56000) primes

getIncrement :: Int -> Maybe Int
getIncrement n
    | ones    = Just $ digitsToIntRev $ map replace digits
    | otherwise = Nothing
    where
        digits = intToDigitsRev n
        ones = 1 `elem` digits
        replace :: Int -> Int
        replace x = if x == 1 then 1 else 0

is8fam :: Int -> Bool
is8fam n = case getIncrement n of
    Nothing -> False
    Just increment -> 
        let fam = map (\x -> n+x*increment) [0..9]
            priemfam = filter isPrime fam
        in length priemfam == 8