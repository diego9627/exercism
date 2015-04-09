{-# LANGUAGE ViewPatterns #-}
module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

number :: String -> String
number (filter isDigit -> str) 
  | length str == 10                   = str
  | length str == 11 && head str == '1'= tail str
  | otherwise                          = "0000000000"

prettyPrint :: String -> String
prettyPrint (number -> [a,b,c,d,e,f,g,h,i,j]) = ['(',a,b,c,')',' ',d,e,f,'-',g,h,i,j]
prettyPrint _                                 = error "Not possible"

areaCode :: String -> String
areaCode (number -> str) = take 3 str

