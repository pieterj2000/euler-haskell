import Data.Ratio
import Data.List ( maximumBy )
import Data.Function (on)


-- gepakt van 64

-- continued fraction van sqrt n
fraction :: Int -> [Int]
fraction n =
    let
        doe :: [Int] -> [(Int, Int, Int)] -> [(Int,(Int, Int, Int))]
        doe as rs =
            let (alpha, beta, gamma) = head rs
                anext :: Int
                anext = let a = fromIntegral alpha
                            b = fromIntegral beta
                            c = fromIntegral gamma
                            k = fromIntegral n
                        in floor $ c / (a*(sqrt k) + b)
                alphanext = alpha*gamma
                betanext = (-alpha)*alpha*n*anext - beta*gamma + beta*beta*anext
                gammanext = alpha*alpha*n - beta*beta
                d = gcd (gcd alphanext betanext) gammanext
                rnext = (alphanext `div` d, betanext `div` d, gammanext `div` d)

                as' = (anext:as)
                rs' = (rnext:rs)
            in if rnext `elem` rs
                then zip as' rs'
                else doe as' rs'

        a = floor $ sqrt $ fromIntegral n
        r = (1,-a,1)

        (as,rs) = unzip $ doe [a] [r]
    in reverse as

fractioncycle = tail . fraction

cyclelength = length . fractioncycle

issquare :: Int -> Bool
issquare n = n == (floor $ sqrt $ fromIntegral n)^2


calc :: [Integer] -> Rational
calc [x] = x % 1
calc (x:xs) = (x % 1) + 1 / (calc xs)

calcConvergent :: Int -> Int -> Rational
calcConvergent d n = calc $ map fromIntegral $ take (n+1) (fraction d ++ cycle (fractioncycle d))

getfundsol :: Int -> (Integer,Integer)
getfundsol n = (x,y)
    where
        cyclelen = cyclelength n
        index = if even cyclelen then cyclelen - 1 else cyclelen * 2 - 1
        f = calcConvergent n index
        x = numerator f 
        y = denominator f

relevants = filter (not . issquare) [2..1000]
result = maximumBy (compare `on` (fst . fst)) $ zip (map getfundsol relevants) relevants