import Primes


test = scanl1 (*) primes

antwoord = last $ takeWhile (<=1_000_000) test