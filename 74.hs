import Digits
import Data.List (unfoldr)
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!?))
import Control.Monad.State.Strict
import Control.Monad (filterM)
import Debug.Trace (traceShow, traceShowM)

type M = State Arr

fact :: [Int]
fact = 1 : scanl1 (*) [1..9]

next :: Int -> Int
next = sum . map (fact !!) . intToDigitsRev

type Arr = M.IntMap Int

hasdepth :: Int -> M (Maybe Int)
hasdepth n = get >>= \arr -> return $ arr !? n

-- zelfde als span, maar dan pakt hij het eerste element waar p niet voor geldt ook nog
span1 :: (a -> Bool) -> [a] -> ([a], [a])
span1 _ xs@[]       = (xs, xs)
span1 p (x:xs)
        | p x       = let (ys,zs) = span1 p xs in (x:ys,zs)
        | otherwise =  ([x],xs)

tilcycle :: Int -> ([Int], [Int])
tilcycle n =
    let doe :: [Int] -> ([Int],[Int])
        doe [] = ([],[])
        doe xs
            | y `elem` xs = (eerst,cycle)
            | otherwise   = doe (y:xs)
            where
                y = next (head xs)
                (cycle, eerst) = span1 (/= y) xs

        lijsts = doe [n]
    in lijsts

makeScore :: ([Int], [Int]) -> [(Int, Int)]
makeScore (eerst, cycle) =
    let cl = length cycle
        eerst' = zip eerst [(cl+1)..]
        cycle' = zip cycle $ repeat cl
    in eerst' ++ cycle'

updatearrvoor :: Int -> M ()
updatearrvoor n =
    let doe :: [Int] -> M [(Int, Int)]
        doe [] = error "poep"
        doe xs = do
            let y = next (head xs)
            d <- hasdepth y
            case d of
                Just dpth -> return $ zip xs [(dpth+1)..]
                Nothing -> do
                    let hascycle = y `elem` xs
                        (cycle, eerst) = span1 (/= y) xs
                        scores = makeScore (eerst,cycle)
                    if hascycle then return scores else doe (y:xs)
    in do
        updates <- doe [n]
        let updates' = M.fromList updates
        arr <- get
        put $ M.union arr updates'

getdepth :: Int -> M Int
getdepth n = do
    d <- hasdepth n
    case d of
        Nothing -> updatearrvoor n >> getdepth n
        Just d' -> return d'

arraysize :: Int
arraysize = (fact !! 9) * 7 + 1
startarray :: Arr
startarray = M.empty

resultaat :: [Int]
resultaat = evalState (filterM (((==60) <$>) . getdepth) [1..999999]) startarray


test = evalState (sequence $ map getdepth $ [145, 169, 69, 540, 1454,363600, 363601]) startarray


main = print resultaat >> print (length resultaat)