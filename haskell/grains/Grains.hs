module Grains (square, total) where

square :: Integer -> Integer
square = (2 ^) . pred

total :: Integer 
total = (sum . map square) [1..64] -- or 2^64 - 1 , aka square 65 - 1
