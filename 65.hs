import Data.Ratio
import Digits

fraction = 2 : let doe n = 1 : (2*n) : 1 : doe (n+1) in doe 1


calc :: [Integer] -> Rational
calc [x] = x % 1
calc (x:xs) = (x % 1) + 1 / (calc xs)


digitsum n = sum $ intToDigitsRev $ numerator $ calc $ take n fraction