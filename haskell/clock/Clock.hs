module Clock (fromHourMin , toString) where

data Clock = Clock Int

reduce :: Int -> Int
reduce = flip mod (day2hour * hour2min)

hour2min :: Int
hour2min = 60

day2hour :: Int
day2hour = 24

instance Show Clock where
  show (Clock n) = let m = reduce n in (showMe (m `div` hour2min)) ++ ":" ++ (showMe (m `mod` hour2min))
    where
      showMe g
        | g <= 9    = "0" ++ show g
	| otherwise = show g

instance Eq Clock where
  (Clock a) == (Clock b) = (reduce a) == (reduce b)

instance Num Clock where
  (Clock a) + (Clock b) = Clock $ reduce (a+b)
  (Clock a) * (Clock b) = Clock $ reduce (a*b)
  abs (Clock a) = Clock $ reduce (abs a)
  signum (Clock a) = Clock $ reduce (signum a)
  negate (Clock a) = Clock $ reduce (negate a)
  fromInteger a = Clock $ reduce (fromInteger a)


fromHourMin :: Int -> Int -> Clock
fromHourMin a b = fromInteger $ toInteger $ a*hour2min + b

toString :: Clock -> String
toString = show
