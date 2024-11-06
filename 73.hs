

nextfarey :: [(Int, Int)] -> [(Int, Int)]
nextfarey [x] = [x]
nextfarey (x@(a,b):y@(c,d):rest) = x : (a+c,b+d) : nextfarey (y:rest)

nextfareylimit :: Int -> [(Int, Int)] -> [(Int, Int)]
nextfareylimit m [x] = [x]
nextfareylimit m (x@(a,b):y@(c,d):rest) = x : if b+d <= m then (a+c,b+d) : nextfareylimit m (y:rest) else nextfareylimit m (y:rest)

start :: [(Int, Int)]
start = [(1,3),(1,2)]


lijsts = iterate nextfarey start
lijstslimit = iterate (nextfareylimit 1200) start

lijsttotfix = map snd $ takeWhile (uncurry (/=)) $ zip lijstslimit $ tail lijstslimit

resultaat = length (last lijsttotfix) - 2

farey :: Int -> [(Int, Int)]
farey 3 = [(1,3),(1,2)]
farey n = 
    let doe :: [(Int, Int)] -> [(Int, Int)]
        doe [x] = [x]
        doe (x@(a,b):y@(c,d):rest)
            | b+d> n    = x : doe (y:rest)
            | b+d <= n  = doe (x:(a+c,b+d):y:rest)
    in doe start
resultaat' = length (farey 12000) - 2

main = print $ length (farey 100000) - 2