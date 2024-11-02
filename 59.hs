import Data.Char 
import Data.Bits(xor)
import Data.List (isInfixOf)

main = do
    input <- readFile "59.cipher.txt"
    let cipher = read input :: [Int]
    let lcc = ['a'..'z']
    let opties = [ [a,b,c] | a<-lcc, b<-lcc, c<-lcc]
    let decodedopties = filter goed $ map (decode cipher . map ord) opties
    let optiesensum = map (\o -> (o, sum $ map ord o, decode (take 3 $ map ord o) (take 3 cipher))) decodedopties
    

    print optiesensum

    return ()


decode :: [Int] -> [Int] -> String
decode code ww = map chr $ zipWith xor (cycle ww) code

goed :: String -> Bool
goed s = all isAscii s && all isPrint s && vooralletters && heeftwoorden && nietveeltekens
    where
        numletters = length $ filter isLetter s
        numtotaal = length s
        vooralletters = 10*numletters >= 7*numtotaal
        numtekens = length $ filter isPunctuation s
        nietveeltekens = 20*numtekens < 1*numtotaal
        heeftwoorden = all (`isInfixOf` s) ["the", "and", "of", "is"]