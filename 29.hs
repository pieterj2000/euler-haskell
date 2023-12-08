import Data.List (nub)


main = print calc

exponentsUnder100 :: Int -> [Int]
exponentsUnder100 a = [1..bound]
    where bound = floor $ logBase aF 100
          aF = fromIntegral a :: Double

powersUnder100 :: Int -> [Int]
powersUnder100 a = map (a^) [1..bound]
    where bound = floor $ logBase aF 100
          aF = fromIntegral a :: Double


calcFor :: Int -> Int
calcFor = length . nub . concatMap (\b -> map (*b) [2..100]) . exponentsUnder100

calc :: Int
calc = sum . map calcFor . filter (`notElem` higherPowers) $ [2..100]
    where higherPowers = concatMap (tail . powersUnder100) [2..100]