module CryptoSquare (normalizePlaintext, squareSize, plaintextSegments, ciphertext, normalizeCiphertext) where

import Data.Char (isAlphaNum, toLower)
import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Data.List (transpose)
import Data.List.Split (chunksOf)

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

squareSize :: String -> Int
squareSize = succ . integerSquareRoot . pred . length

plaintextSegments :: String -> [String]
plaintextSegments = (chunksOf =<< squareSize) . normalizePlaintext

ciphertext :: String -> String
ciphertext = concat . precipher

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . precipher

precipher :: String -> [String]
precipher = transpose . plaintextSegments


