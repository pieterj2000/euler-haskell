import Digits
import Control.Monad (guard)
import Data.List (sort)

main = print calc

calc = sum possibilities

possibilities :: [Int]
possibilities = do
    let r xs x = filter (/=x) xs
    d4 <- [0,2,4,6,8]
    d6 <- r [0,5] d4
    let rem3 = filter (`notElem` [d4,d6]) [0..9]
    d3 <- rem3
    let rem5 = r rem3 d3
    d5 <- rem5
    let rem7 = r rem5 d5
    d7 <- rem7
    let rem8 = r rem7 d7
    d8 <- rem8
    let rem9 = r rem8 d8
    d9 <- rem9
    let rem10 = r rem9 d9
    d10 <- rem10
    let rem1 = r rem10 d10
    d1 <- rem1
    let rem2 = r rem1 d1
    d2 <- rem2

    guard ((d3+d4+d5) `mod` 3 == 0)
    guard (digitsToInt [d5,d6,d7] `mod` 7 == 0)
    guard (digitsToInt [d6,d7,d8] `mod` 11 == 0)
    guard (digitsToInt [d7,d8,d9] `mod` 13 == 0)
    guard (digitsToInt [d8,d9,d10] `mod` 17 == 0)

    let ls = [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10]
    --guard (sort ls == [1..9])
    return $ digitsToInt ls