
main = print $ calc

s :: Int -> Int
s 1 = 1
s n = if even n then error "even n" else s (n-2) + 4*(n-2)*(n-2) + 10*(n-1)

s' :: Int -> Int
s' 0 = 1
s' n = s' (n-1) + 4*(2*n-1)^2 + 20*n

s'' :: Int -> Int
s'' n = 1 + sum [16*j*j + 4*j + 4 | j<-[1..n]]

s''' :: Int -> Int
s''' n = 1 + (2*n*(8*n*n+13)) `div` 3 + 10*n*n

calc = s''' 500