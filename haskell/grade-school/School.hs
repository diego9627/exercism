{-# LANGUAGE ViewPatterns #-}
module School(empty,add,sorted,grade,School) where

import qualified Data.Map as M
import Data.List (insert)
newtype School = Id {runId :: M.Map Int [String]}

empty :: School
empty = Id (M.empty)

sorted :: School -> [(Int,[String])]
sorted = M.toList . runId

add :: Int -> String -> School -> School
add n str (runId -> s) = Id $ M.alter concatOrThat n s
  where
    concatOrThat mxs = case mxs of 
      Nothing -> Just [str]
      Just xs -> Just (insert str xs)

grade :: Int -> School -> [String]
grade n (runId -> s) = case M.lookup n s of
  Nothing -> []
  Just xs -> xs
