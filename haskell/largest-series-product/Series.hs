module Series (digits, slices, largestProduct) where

import Data.Char (digitToInt)
import Control.Arrow

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n str
  | n <= 0 || n > len = []
  | otherwise         = splices (len - n + 1) (digits str)
    where
      len       = length str
      splices 0 = const []
      splices d = uncurry (:) . second (splices (d - 1)) . (take n &&& tail)

largestProduct :: Int -> String -> Int
largestProduct n = maximum . (\s -> if null s then [1] else s) . map product . slices n
