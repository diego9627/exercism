module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear n = (mod n 400 == 0) || (mod n 4 == 0 && mod n 100 /= 0)
