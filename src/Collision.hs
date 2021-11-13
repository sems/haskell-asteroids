module Collision where

import Model
  (Asteroid(Asteroid, asteriodPos), 
  Player(Player, lives),
  GameState(GameState,player1,player2,asteroids,bullets, collision),
  Bullet(Bullet),
  Time,Position)

import View(bulletPath, playerPath)
import Constants(baseSize)

import qualified Graphics.Gloss.Data.Point.Arithmetic  as A ((-)) 
import Graphics.Gloss.Geometry.Line(closestPointOnLine)
import Graphics.Gloss.Data.Vector (dotV, angleVV, argV)

-- checks if a given player and asteroid collide and return a bool by checking for collision between the asteroids and each line of the player
checkPlayerCollision :: Asteroid -> Player -> Bool
checkPlayerCollision _ (Player 0 _ _ _) = False -- if an player is death they automatically don't collide
checkPlayerCollision a@(Asteroid pos@(ax,ay) _ s _) pl =   any (checkSegCollision a) (getsegments $ playerPath pl)
  where getsegments[x,y,z] = [(x,y),(y,z),(x,z)]  -- distibute path into line segments
        
  -- check if a line intersects with an asteroids (or is inside of it) and return a bool
checkSegCollision :: Asteroid -> (Position,Position) -> Bool
checkSegCollision (Asteroid pos@(ax,ay) _ s _) seg  =  doesCollide $ getdistance $ getclosest pos seg
  where getclosest p (x,y) | dotV xy yp > 0 = y    -- get from each line segemnt the closest point to mid asteroid (here x,y aree two points and not coordinates)
                           | dotV xy xp < 0 = x       --
                           | otherwise = closestPointOnLine x y p -- used function only looks for the closest point on a infinate line so only used if said closest point of 
          where xy = x A.- y                                         -- the infinate line is also within segment (which is the case when the needed point isnt either x or y itself)
                xp = x A.- p
                yp = y A.- p
        getdistance (x,y) = sqrt ((x - ax)*(x-ax)+ (y-ay)*(y-ay)) -- get distance between point and mid asteroid
        doesCollide x = x <= fromIntegral(s* baseSize) --if the distance between the line and midpoint of the asteroids is smaller than the asteroids they collide

-- for each bullet and asteroid check if there is a collision and return an updated gamestate if nessisary
handleBulletCollision :: GameState -> GameState
handleBulletCollision gstate = checkCollision (asteroids gstate) (bullets gstate) [] []
  where 
    checkCollision:: [Asteroid] -> [Bullet] -> [Asteroid] -> [Bullet] -> GameState
    checkCollision _ [] _ _ = gstate
    checkCollision [] (b:bs) aRest bRest = checkCollision aRest bs [] (b:bRest)
    checkCollision (a:as) (b:bs) aRest bRest | checkSegCollision a (bPoints(bulletPath b)) = gstate{asteroids = as++aRest, bullets = bs ++ bRest,collision = (asteriodPos a,1.0):collision gstate}
                                             | otherwise = checkCollision as (b:bs) (a:aRest) bRest   --if there is a collision both asteroid and bullet will be removed and an animation will be played on the asteroid
     where bPoints [x,y] = (x,y)


-- handle the collision between the player and asteroids
handleCollision :: GameState -> GameState
handleCollision gstate@(GameState _ p1 p2 astrs _ _ _ col) = loseLife (collisionWith astrs [] Nothing)
  where
    collisionWith :: [Asteroid] -> [Asteroid] -> Maybe Player -> ([Asteroid], Maybe (Player, Asteroid)) -- returns the colliding player and asteroid(if there is one) and an updated version of the asteroids list(with the colliding asteroids removed)
    collisionWith [] ys may = (ys, Nothing)
    collisionWith (x:xs) ys may | checkPlayerCollision x p1 = (xs ++ ys, Just (p1, x)) 
                                | checkPlayerCollision x p2 = (xs ++ ys, Just (p2, x))
                                | otherwise = collisionWith xs (x:ys) may
    loseLife :: ([Asteroid], Maybe (Player, Asteroid)) -> GameState -- updates the gamestate so that the colliding players lives decreases, asteroids list is updated, and the colliding asteroids playes an animation
    loseLife (as, may) = case may of
      Nothing -> gstate
      Just (player, astr@(Asteroid pos _ _ _)) | player == p1 -> gstate{player1 = p1{lives = lives p1 - 1}, asteroids = as, collision = (pos, 1.0) : col}
                                               | player == p2 -> gstate{player2 = p2{lives = lives p2 - 1}, asteroids = as, collision = (pos, 1.0) : col}

-- Calculates the duration of each animation left
handleCollision' :: Time -> GameState -> GameState
handleCollision' secs gstate@(GameState _ _ _ _ _ _ _ cols) = gstate{collision = collision'} 
  where
    -- First maps the current time over the whole list of currect collisions/animations
    -- Then it will filters all out which animations are finished, to be on the safeside -2 seconds.
    collision' :: [(Position, Time)]
    collision' = filter ff $ map mf cols 
      where
        mf :: (Position, Time) -> (Position, Time)
        mf (pos, time) = (pos, time - secs)
        ff :: (Position, Time) -> Bool
        ff (pos, time) = time >= -2