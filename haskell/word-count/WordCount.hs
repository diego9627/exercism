{-# LANGUAGE ViewPatterns #-}
module WordCount where

import qualified Data.Map as M
import Data.Char (isAlphaNum,toLower)

wordCount :: String -> M.Map String Int
wordCount ((words . clean) -> strs) = foldr (M.alter plusOne) M.empty strs
  where
    plusOne Nothing  = Just 1
    plusOne (Just s) = Just (s + 1)

clean :: String -> String
clean            = map (\c -> if isAlphaNum c then toLower c else ' ')

