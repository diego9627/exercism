{-# LANGUAGE ViewPatterns #-}
module DNA (count, nucleotideCounts) where

import Data.Map (fromList,Map,adjust)

count :: Char -> String -> Int
count c (clean -> str) 
  | elem c "ATCG" = length . filter (== c)  $ str
  | otherwise     = error $  "invalid nucleotide '" ++ (c:"'") 


nucleotideCounts :: String -> Map Char Int
nucleotideCounts (clean -> str) = foldr (adjust succ) (fromList $ zip "ATCG" [0,0..] ) str

clean :: String -> String
clean = foldr (\x -> if (elem x "ATCG") then (x:) else (error $ "invalid nucleotide '" ++ (x:"'"))) ""

