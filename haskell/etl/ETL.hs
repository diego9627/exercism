module ETL (transform) where

import qualified Data.Map.Lazy as M
import Data.Char (toLower)

transform :: M.Map Int [String] -> M.Map String Int
transform = M.foldrWithKey (flip . foldr . flip (M.insert . map toLower)) M.empty
