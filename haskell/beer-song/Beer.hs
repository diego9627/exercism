module Beer (sing, verse) where

sing :: Int -> Int -> String
sing b a = (unlines . map verse) [b,b-1 .. a]

verse :: Int -> String 
verse 0 = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
verse 1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
verse n = (bottle n) ++ " on the wall, " ++ (bottle n) ++ ".\nTake one down and pass it around, " ++ (bottle (n-1)) ++ " on the wall.\n"
  where
    bottle 0 = "no more bottles of beer"
    bottle 1 = "1 bottle of beer"
    bottle k = (show k) ++ " bottles of beer"

