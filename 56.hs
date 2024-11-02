import Digits

f :: Integer -> Integer -> Integer
f a b = tel (a^b)

tel :: Integer -> Integer
tel n = sum (intToDigitsRev n)

result = maximum [ f a b | a <- [90..99], b<-[90..99]]