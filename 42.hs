import Data.Char (ord)

main = do
    wordsInput <- readFile "42.words.txt"
    let words = read ("[" ++ wordsInput ++ "]") :: [String]
    print $ calc words

calc = length . filter isTriangle' . map wordToSum

wordToSum :: String -> Int
wordToSum = sum . map letterToInt

letterToInt :: Char -> Int
letterToInt = subtract (ord 'A' - 1) . ord

isTriangle :: Int -> Bool
isTriangle x = let n = floor (sqrt $ fromIntegral $ 2*x) :: Int in (n*(n+1)) `div` 2 == x

isTriangle' :: Int -> Bool
isTriangle' x = isInt (sqrt $ 8*(fromIntegral x) + 1)

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)