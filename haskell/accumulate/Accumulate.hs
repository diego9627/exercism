module Accumulate where


accumulate :: (a -> b) -> [a] -> [b]
accumulate = flip foldr [] . (.) (:) 
