module GSWH

where 
import List
import Char

ld n = ldf 2 n 

divides d n = rem n d == 0 

ldf k n | divides k n = k 
        | k^2 > n     = n 
        | otherwise   = ldf (k+1) n

prime n | n < 1     = error "not a positive integer"
        | n == 1    = False 
        | otherwise = ld n == n

somePrimes    = filter prime [1..1000]

primesUntil n = filter prime [1..n]

allPrimes     = filter prime [1..]
