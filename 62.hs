import qualified Data.Map.Strict as M
import Digits
import Data.List (sort)

calc :: M.Map [Int] [Int] -> Int -> [Int]
calc map n =
    let cube = n*n*n
        key = sort $ intToDigitsRev cube
        (val, map') = M.insertLookupWithKey (\key newcube oldcubes ->
                newcube ++ oldcubes
            ) key [cube] map
        (klaar, result) = case val of
            Nothing -> (False, undefined)
            Just cs -> if length cs == 4
                then (True, cube:cs)
                else (False, undefined)
    in if klaar
        then result
        else calc map' (n+1)

resultaat = minimum $ calc M.empty 1