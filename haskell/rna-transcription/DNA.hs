module DNA where

toRNA :: String -> String
toRNA = map swap
  where
    swap 'G' = 'C' 
    swap 'C' = 'G' 
    swap 'T' = 'A' 
    swap 'A' = 'U'
    swap _   = error "Your cases are wrong"
