{-# LANGUAGE ViewPatterns #-}
module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

data Schedule = Teenth | First | Second | Third | Fourth | Last deriving (Show, Eq)
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Enum, Eq)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay sch wd year month = (head . 
                              filter ((wd ==) . toEnum . pred . snd . mondayStartWeek ) . 
			      map (fromGregorian year month) . 
			      map (+ (offset sch))) [1..7]
			        where 
			          offset s = case s of
				    First  -> 0
				    Second ->  7
				    Third  -> 14
				    Fourth -> 21
				    Teenth -> 12
				    Last   -> ((gregorianMonthLength year month) - 7)
