module Triangle where

data TriangleType = Equilateral | Isosceles | Scalene | Illogical deriving (Show, Eq)


triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c 
  | a <= 0 || b <= 0 || c <= 0 || a + b <= c || b + c <= a || c + a <= b = Illogical
  | a == b && b == c = Equilateral
  | a == b || b == c || c == a = Isosceles
  | otherwise = Scalene
