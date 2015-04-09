{-# LANGUAGE ViewPatterns #-}
module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor (map toLower -> lx) = filter (\y -> lx /= map toLower y && sorted == sort (map toLower y))
  where
    sorted = sort (map toLower lx)
