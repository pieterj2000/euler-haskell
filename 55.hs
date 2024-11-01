import Digits
import Data.List (unfoldr)


f :: Integer -> Integer 
f n = n + (digitsToInt . intToDigitsRev) n

flist = iterate f


f' :: Integer -> Maybe (Integer, Integer)
f' n =  let nr = (digitsToInt . intToDigitsRev) n
            nn = nr + n
        in if nr == n then Nothing else Just (nn,nn)

flist' n = let nn = f n in nn : unfoldr f' nn

isLychrel :: Integer -> Bool
isLychrel n = length list == 50
    where
        list = take 50 $ flist' n

result = length $ filter isLychrel [1..9999]