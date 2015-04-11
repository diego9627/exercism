module Raindrops (convert) where

import Data.Monoid

convert :: Integer -> String
convert n = maybe (show n) id ((mconcat . map (Just . snd) . filter ((==0) . mod n . fst)) list)
  where
   list = [(3,"Pling"),(5,"Plang"),(7,"Plong")]
