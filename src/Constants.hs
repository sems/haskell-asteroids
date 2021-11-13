module Constants where

--playerspeed 
pS:: Float 
pS = 75

--directionSpeed
dS :: Float
dS = 10

--asteroidspeed
aS :: Float 
aS = 500

-- explosionSize
eS :: Float
eS = 20.0

--basesize of asteroid
baseSize :: Int 
baseSize = 5

-- inRange Function
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

bHeight :: Float
bHeight = 112; -- Height of a button 