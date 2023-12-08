import Data.List (foldl1')
main = print calc


digitOptions :: Int -> Int -> [Int]
digitOptions goal coefficient = filter (\x -> coefficient * x <= goal) [0..9]

options :: Int -> Int -> [[Int]]
options _ 0 = [[]]
options goal numDigits = do
    let coef = 10^(numDigits - 1)
    a <- digitOptions goal coef
    next <- options (goal - a*coef) (numDigits - 1)
    return (a:next)

options' :: Int -> Int -> [Int]
options' _ 0 = [0]
options' goal numDigits = do
    let coef = 10^(numDigits - 1)
    a <- digitOptions goal coef
    next <- options' (goal - a*coef) (numDigits - 1)
    return (a * coef + next)

options'' :: Int -> Int -> [(Int, [Int])]
options'' _ 0 = [(0, [])]
options'' goal numDigits = do
    let coef = 10^(numDigits - 1)
    a <- digitOptions goal coef
    (nextInt, nextDigits) <- options'' (goal - a*coef) (numDigits - 1)
    return (a * coef + nextInt, a:nextDigits)

digitsToInt :: [Int] -> Int
digitsToInt = sum . map (^5)

calc :: Int
calc = sum $ drop 2 $ map fst $ filter (\(x,ds) -> digitsToInt ds == x) $ options'' (6*9^5) 6