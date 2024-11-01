import Digits
import Data.List (sort)


sameDigits :: Int -> Int -> Bool
sameDigits a b = sort (intToDigits a) == sort (intToDigits b)

isgoed :: Int -> Bool
isgoed a =  let nums = map (*a) [2..6]
            in all (sameDigits a) nums


result = head $ filter isgoed [1..]
