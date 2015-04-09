module Bob (responseFor) where

import Data.Char
import Control.Applicative
import Data.List

responseFor :: String -> String
responseFor str 
  | all ((==>) <$> isAlpha <*> isUpper) str && any isAlpha str = "Whoa, chill out!"
  | all isSpace                                            str = "Fine. Be that way!"
  | isSuffixOf  "?"                                        str = "Sure."
  | otherwise                                                  = "Whatever."


(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> y = y

