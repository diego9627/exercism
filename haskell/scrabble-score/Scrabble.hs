{-# LANGUAGE ViewPatterns #-}
module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Int
scoreLetter (toUpper -> c)
  | c `elem` "AEIOULNRST" = 1
  | c `elem` "DG" = 2
  | c `elem` "BCMP" = 3
  | c `elem` "FHVWY" = 4
  | c `elem` "K" = 5
  | c `elem` "JX" = 8
  | c `elem` "QZ" = 10
  | otherwise = error $ (show c) ++ " is not a letter."

scoreWord :: String -> Int 
scoreWord = sum . map scoreLetter

