module Constants where

--playerspeed 
pS:: Float 
pS = 50

--directionSpeed
dS :: Float
dS = 10

--asteroidspeed
aS :: Float 
aS = 500

baseSize :: Int --basesize of asteroid
baseSize = 5

-- inRange Function
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

data MoveDirection = UpDir | DownDir | LeftDir | RightDir
  deriving Eq