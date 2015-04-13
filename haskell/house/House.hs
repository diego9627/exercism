module House (rhyme) where

import Data.List (tails)

rhyme :: String
rhyme = concatMap verse combined
  where 
    actors  = ["house that Jack built","malt","rat","cat","dog","cow with the crumpled horn","maiden all forlorn","man all tattered and torn","priest all shaven and shorn","rooster that crowed in the morn","farmer sowing his corn","horse and the hound and the horn"]
    actions = ["lay in","ate","killed","worried","tossed","milked","kissed","married","woke","kept","belonged to",""]
    this p =  concat ["This is the ", p]
    that (p,a) = concat ["\nthat ",a, " the ", p]
    combined = tail $ reverse $ tails $ reverse $ zip actors actions
    verse []         = error ""
    verse ((x,_):xs) = concat (this x:map that xs) ++ ".\n\n"
