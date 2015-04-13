module Matrix ( Matrix, row, column, rows, cols, shape, transpose , reshape, flatten, fromString, fromList) where

import Control.Lens
import Data.Vector.Lens
import qualified Data.Vector as V
import Control.Arrow
import Data.List.Split (chunksOf)

newtype Matrix a = M (V.Vector (V.Vector a)) deriving (Show, Eq)

row :: Int -> Matrix a -> V.Vector a
row n (M m) = m ^. ix n

column :: Int -> Matrix a -> V.Vector a
column n (M m) = m & toVectorOf (traverse . ix n)

rows :: Matrix a -> Int
rows (M m) = m & V.length

cols :: Matrix a -> Int
cols (M m) = maybe 0 id (m ^? ix 0 & fmap V.length)

shape :: Matrix a -> (Int,Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a 
transpose (M m) = m ^.. (traverse.from vector) & transposeOf traverse & fromList

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_,c) mtx = mtx & flatten & view (from vector) & chunksOf c & fromList

flatten :: Matrix a -> V.Vector a
flatten (M m) = m ^. traverse

fromString :: Read a => String -> Matrix a
fromString = fromList . map stringToList . lines
  where stringToList str = case reads str of
          [(v,rest)] -> v:stringToList rest
          _          -> []

fromList :: [[a]] -> Matrix a
fromList  = M . toVectorOf (traverse.vector) 
