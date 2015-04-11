module Queens (boardString, canAttack) where

import Control.Lens

type Queen = (Int,Int)

boardString :: Maybe Queen -> Maybe Queen -> String
boardString w b = clean & posTo 'W' w & posTo 'B' b & concat
  where
    posTo _ Nothing      = id
    posTo c (Just (x,y)) = ix x.ix (2*y) .~ c

canAttack :: Queen -> Queen -> Bool
canAttack (x,y) (z,w) = (x == z) || (y == w) || (x + y == z + w) || (x - y == z - w)

clean :: [String]
clean = ["_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n",
         "_ _ _ _ _ _ _ _\n"]

