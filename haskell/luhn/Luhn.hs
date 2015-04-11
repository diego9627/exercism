module Luhn (checkDigit, addends, checksum, isValid, create) where

import Data.Char (digitToInt)

checkDigit :: Integer -> Integer
checkDigit = flip mod 10

addends :: Integer -> [Integer]
addends = reverse . zipWith ($) (cycle [id,otherRule]) . reverse . map (toInteger . digitToInt) . show
  where
    otherRule n = if n >= 5 then 2*n - 9 else 2*n

checksum :: Integer -> Integer
checksum = flip mod 10 . sum . addends

isValid :: Integer -> Bool
isValid = (==0) . checksum

create :: Integer -> Integer
create n = 10*n + mod (10 - checksum (10*n)) 10
