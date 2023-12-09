import qualified Data.Array as A
import Data.Array ((!))


main = print calc



calc :: Int
calc = options 200 7

options :: Int -> Int -> Int
options _ 0 = 1  -- There is always only one way to get an amount using only 1ct coins
options 0 _ = 1  -- There is always only one way to get an amount of 0
options amount coins = let  curVal = coinValues !! coins
                            possibleBiggest = [0..(floor $ (fromIntegral amount) / (fromIntegral curVal))]
                        in sum $ map (\x -> optionsArr ! (amount - x*curVal, coins - 1))  possibleBiggest

coinValues :: [Int]
coinValues = [1,2,5,10,20,50,100,200]

optionsArr :: A.Array (Int, Int) Int
optionsArr = A.array ((0,0),(200,length coinValues)) [ (i, options amount coins) | i@(amount,coins) <- A.range ((0,0),(200,length coinValues)) ]