module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr (\x-> (if p x then (x:) else id)) [] 


discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)
