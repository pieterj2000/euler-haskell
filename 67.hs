import Data.List (foldl')



main = do
    input <- readFile "67.triangle.txt"
    let regelsstring = lines input
        regels = map (map read . words) regelsstring :: [[Int]]
    print $ findmax regels

    return ()

findmax = maximum . dolines

dolines :: [[Int]] -> [Int]
dolines = foldl' doline []


doline :: [Int] -> [Int] -> [Int]
doline boven onder = zipWith (+) onder boven'
    where boven' = zipWith max (0:boven) (boven ++ [0])