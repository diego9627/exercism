module Binary (toDecimal) where

import Data.Char (digitToInt)

toDecimal :: String -> Int
toDecimal str
  | all (flip elem "01") str = toDecimal' str
  | otherwise = 0

toDecimal' :: String -> Int
toDecimal' = sum . zipWith (*) (iterate (*2) 1) . reverse . map digitToInt

