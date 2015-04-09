module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

import Data.List

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show,Eq)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys = case (properSublist xs ys, properSublist ys xs) of
  (True, True ) -> Equal
  (True, False) -> Sublist
  (False,True ) -> Superlist
  (False,False) -> Unequal

properSublist :: Eq a => [a] -> [a] -> Bool 
properSublist xs [] = xs == []
properSublist xs ls@(_:ys) = isPrefixOf xs ls || properSublist xs ys
