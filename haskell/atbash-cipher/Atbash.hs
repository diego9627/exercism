module Atbash (encode) where

import Data.Char
import Control.Monad (liftM2)
import Data.List.Split (chunksOf)
import Data.List 

encode :: String -> String 
encode = concat . intersperse " " . chunksOf 5 . map (swap . toLower) . filter (liftM2 (&&) isAscii isAlphaNum)
  where
    swap c
      | isAlpha c = chr (ord 'a' + ord 'z' - ord c)
      | otherwise = c
