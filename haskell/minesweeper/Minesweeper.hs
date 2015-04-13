module Minesweeper (annotate) where


import qualified Data.Vector as V
import Data.Vector.Lens
import Control.Lens
import Data.Char

annotate :: [String] -> [String]
annotate = fromBoard .  . toBoard

toBoard :: [String] -> V.Vector (V.Vector (Maybe Int))
toBoard ss = (ss & traverse.traverse %~ op) ^.. traverse . vector ^. vector
  where 
    op c 
      | isDigit c = Just (digitToInt c)
      | otherwise = Nothing

fromBoard :: V.Vector (V.Vector (Maybe Int)) -> [String]
fromBoard bd = (bd & traverse.traverse %~ antiop) ^.. traverse.from vector
  where
    antiop mi = case mi of
      Nothing -> '*'
      Just  x -> intToDigit x
