module SpaceAge (Planet(..), ageOn) where


data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

earthSeconds :: Float
earthSeconds = 31557600

ageOn :: Planet -> Float -> Float 
ageOn = (flip (/)) . (*earthSeconds) . convert

convert :: Planet -> Float
convert Earth   = 1
convert Mercury = 0.2408467 
convert Venus   = 0.61519726 
convert Mars    = 1.8808158 
convert Jupiter = 11.862615 
convert Saturn  = 29.447498 
convert Uranus  = 84.016846 
convert Neptune = 164.79132 

