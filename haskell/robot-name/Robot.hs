module Robot (robotName, mkRobot, resetName) where

import System.Random
import Data.IORef
import Control.Applicative

data Robot = Robot {name :: String}

mkRobot :: IO (IORef Robot)
mkRobot = do
  nn <- randomSerial
  newIORef (Robot nn)

robotName :: IORef Robot -> IO String
robotName r =  name <$> (readIORef r)

resetName :: IORef Robot -> IO ()
resetName r = do
  nn <- randomSerial
  writeIORef r (Robot nn)

randomSerial :: IO String
randomSerial = do
  g <- newStdGen
  let l = take 2 $ randomRs ('A','Z') g
  let n = take 3 $ randomRs ('0','9') g
  return (l ++ n)
