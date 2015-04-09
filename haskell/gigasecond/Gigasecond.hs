module Gigasecond (fromDay) where

import Data.Time.Clock

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime (1000000000 :: NominalDiffTime) 
