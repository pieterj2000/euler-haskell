import Data.Ratio (denominator, numerator, (%))
import Digits (numdigits, numdigitswithlog)

f :: Rational -> Rational
f q = (a+2*b) % (a+b)
    where
        a = numerator q
        b = denominator q

flist = tail $ iterate f 1

flist' = take 1000 flist

isgoed :: Rational -> Bool
isgoed q = numdigits a > numdigits b
    where
        a = numerator q
        b = denominator q

result = length $ filter isgoed $ flist'