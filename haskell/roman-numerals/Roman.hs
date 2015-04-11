module Roman (numerals) where

import Data.Char (digitToInt)

numerals :: Int -> String
numerals = concat . map (uncurry digpos) . reverse . zip [0..] . map digitToInt . reverse . show

digpos :: Int -> Int -> String
digpos p n
  | n <= 3 = replicate n (tens p)
  | n == 4 = [tens p,fives p]
  | n == 5 = [fives p]
  | n <= 8 = (fives p):(replicate (n - 5) (tens p))
  | n == 9 = [tens p , tens (p + 1) ]
  | otherwise = error "Not a digit"

tens :: Int -> Char
tens n = case n of
  0 -> 'I'
  1 -> 'X'
  2 -> 'C'
  3 -> 'M'
  _ -> error $ "I don't know the letter."

fives :: Int -> Char
fives n = case n of 
  0 -> 'V'
  1 -> 'L'
  2 -> 'D'
  _ -> error $ "I don't know the letter."
