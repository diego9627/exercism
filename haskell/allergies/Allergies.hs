module Allergies where

import Data.Bits (testBit)

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats deriving (Show, Eq, Enum)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = testBit n (fromEnum a)

allergies :: Int -> [Allergen]
allergies n = filter (flip isAllergicTo n) (enumFromTo Eggs Cats)
