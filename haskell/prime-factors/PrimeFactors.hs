module PrimeFactors (primeFactors) where

import Data.Numbers.Primes (primes)

primeFactors :: Integer -> [Integer]
primeFactors = factor primes
  where
    factor l@(p:ps) n 
      | mod n p == 0 = p:factor l (div n p)
      | n == 1       = []
      | otherwise    = factor ps n
    factor [] _ = error "error"
