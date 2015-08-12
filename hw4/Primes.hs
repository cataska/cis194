module Primes where

import Data.List

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =  map (\x -> x * 2 + 1) filtered
    where
      filtered = filter (\x -> x `notElem` toRemoved) [1..n]
      toRemoved = [ i+j+2*i*j | j <- [1..n],
                                i <- [1..j],
                                (i+j+2*i*j) <= n ]
