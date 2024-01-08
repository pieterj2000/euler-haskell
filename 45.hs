import Digits
import Control.Monad (guard)
import Data.List (sort)
import Data.Maybe (mapMaybe)

main = print calc

calc = (map ktox $ filter ktogoodl [0..]) !! 2

ktox :: Int -> Int
ktox k = 2*k*k+3*k+1

ktogoodl :: Int -> Bool
ktogoodl k' =
    let k = fromIntegral k'
        l' = ( sqrt (48*k*k+72*k+25) - 5 ) / 6
    in isInt l'

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)