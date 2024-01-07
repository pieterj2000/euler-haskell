import Digits

main = print calc

calc = product $ map iDigit $ map (10^) $ [0..6]
--calc = possibleTriples 120

lengteSectie :: Int -> Int
lengteSectie (-1) = 0
lengteSectie n = 9*(10^n)*lengtePerSectie n + lengteSectie (n-1)

lengtePerSectie :: Int -> Int
lengtePerSectie = (+1)

iInSectie :: Int -> Int -> Int
iInSectie n i =
    let (hoeveelstegetal, digit) = (i-1) `divMod` lengtePerSectie n
        getal = 10^n + hoeveelstegetal
    in intToDigits getal !! digit

iDigit :: Int -> Int
iDigit i = let n = getSectie i in iInSectie n (i - lengteSectie (n-1))

getSectie :: Int -> Int
getSectie i = head $ dropWhile ((i>) . lengteSectie) [0..]