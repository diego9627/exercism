module Squares (sumOfSquares, squareOfSums, difference) where

import Control.Monad (liftM2)

squareOfSums :: Integral a => a -> a
squareOfSums n = let m = ((n*(n+1)) `div` 2) in m*m

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (n*(n+1)*(2*n+1)) `div` 6

difference :: Integral a => a -> a 
difference = liftM2 (-) squareOfSums sumOfSquares

