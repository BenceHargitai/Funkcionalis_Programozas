module Lesson where

--2 hanyadik hatványa a 10 a 20.on
-- head [ (2 ^ n, n) | n <- [1..], 2^n > 10^20]

--első 10 elem:
-- take 10 [ (2 ^ n, n) | n <- [1..], 2^n > 10^20]
--head: üres listánál nem működik, take igen

--egy adott érték prím-e


isPrime :: Integral a => a -> Bool
-- isPrime n = length (divisors n) == 2
-- Itt csak az első két elemig nézi ˇ
isPrime n = divisors n == [1,n] 
-- viszont így az 1 is prím lesz szerinte szóval:
isPrime 1 = False
isPrime n = null (divisors n) --megnézi h üreslista-e

--HF :

divisors :: Integral a=> a -> [a]
-- divisors n = [i | i <- [1..n], n `mod` i == 0 ] 
--gyorsított verzió:
divisors n = [i | i <- [1..n `div` 2] , n `mod` i == 0 ] 

primes :: Integral a => [a]
primes = [ i| i <- [1..], isPrime i]
