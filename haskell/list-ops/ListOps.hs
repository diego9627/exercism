module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b []     = b
foldl' f b (a:as) = let z = f b a in seq z (foldl' f z as)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b []     = b
foldr f b (a:as) = f a (foldr f b as)

length :: [a] -> Int
length = foldr (const succ) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f= foldr ((:).f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x -> if p x then (x:) else id) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
