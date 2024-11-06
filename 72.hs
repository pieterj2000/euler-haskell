import Primes
import Data.List (nub)
import Data.Ratio
import qualified Data.IntMap.Lazy as M
import Data.IntMap.Lazy ((!))

totientmap :: M.IntMap Int
totientmap = eulerTotientMapMaxSize 1_000_000

tm :: Int -> Int
tm n = sum $ map (totientmap ! ) [2..n]

tm' :: Int -> Int
tm' n = length . nub $ [ a % b | b<-[2..n], a<-[1..(b-1)]]

result = tm 1_000_000

main = print result 