module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import Control.Lens
import Data.Maybe
import Data.List
import Data.List.Split

data Plant = Grass | Clover | Radishes | Violets deriving (Show, Eq)

garden :: [String] -> String -> [(String,[Plant])]
garden names str = str & break (=='\n') & _2 %~ tail & both %~ (splitPlaces (repeat 2)) & uncurry (zipWith (++)) & zip (sort names) & mapped._2.mapped %~ convert
  where
     convert c = fromJust $ lookup c [('G',Grass),('C',Clover),('R',Radishes),('V',Violets)]
  
  
defaultGarden :: String -> [(String,[Plant])]
defaultGarden = garden ["Alice","Bob","Charlie","David"
                       ,"Eve","Fred","Ginny","Harriet"
		       ,"Ileana","Joseph","Kincaid","Larry"]

lookupPlants :: String -> [(String,[Plant])] -> [Plant]
lookupPlants = (maybe [] id .) . lookup
