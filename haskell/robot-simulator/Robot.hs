module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft) where

data Bearing = North | East | South | West deriving (Show, Eq, Enum)
data Robot = Robot { bearing :: Bearing , coordinates :: (Int,Int) } deriving (Show, Eq)

mkRobot :: Bearing -> (Int, Int) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate robot str = (foldr (.) id (map toOp (reverse str))) robot
  where
    toOp c = \(Robot d (x,y)) -> case c of
      'L' -> Robot (turnLeft  d) (x,y)
      'R' -> Robot (turnRight d) (x,y)
      'A' -> Robot d (case d of
        East  -> (x+1,y)
	North -> (x,y+1)
	West  -> (x-1,y)
	South -> (x,y-1))
      _   -> Robot d (x,y)
        
turnRight :: Bearing -> Bearing
turnRight = turn 1

turnLeft :: Bearing -> Bearing
turnLeft = turn 3

turn :: Int -> Bearing -> Bearing
turn n = toEnum . flip mod 4 . (+n) . fromEnum


