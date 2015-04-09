module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

sumOfMultiplesDefault :: Integer -> Integer
sumOfMultiplesDefault = sumOfMultiples [3,5]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples xs n = sumOfMultiples' xs [1..n-1]

sumOfMultiples' :: [Integer] -> [Integer] -> Integer
sumOfMultiples' xs ys = sum $ filter (\y -> any (\x -> mod y x == 0) xs) ys
