module Hexadecimal (hexToInt) where

import Data.Char

hexToInt :: String -> Int
hexToInt = maybe 0 id . fmap (sum . zipWith (*) (iterate (*16) 1) . reverse) . sequence . map op
  where
    op c 
      | isDigit c = Just $ ord c - ord '0'
      | isLower c && ord c <= ord 'f' = Just $ ord c - ord 'a' + 10
      | otherwise = Nothing
