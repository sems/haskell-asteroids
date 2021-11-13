{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Movement where

import Constants (  pS, dS, aS, (<?) )
import Model
    ( Asteroid(Asteroid, asteriodPos),
      Player(Player, playerDir, playerPos),
      GameState(player2, player1, keys, asteroids, bullets),
      Direction,
      Bullet(Bullet, bulletPos, bulletDir),
      MoveDirection(..))

import Graphics.Gloss.Interface.IO.Game (Key (Char, SpecialKey), SpecialKey (KeyDown, KeyLeft, KeyUp, KeyRight))
import qualified Graphics.Gloss.Data.Point.Arithmetic  as A ((+))
import Graphics.Gloss.Data.Vector ( mulSV )

import qualified Data.Set as S

-- updated the gamestate with the new asteroid positions
moveAsteroids :: Float -> GameState -> GameState
moveAsteroids secs gstate = gstate { asteroids = asteroids' }
  where
    asteroids':: [Asteroid]
    asteroids' = map (moveAsteroid' secs) $ asteroids gstate

--handles the movement of an individual asteroids
moveAsteroid' :: Float -> Asteroid -> Asteroid
moveAsteroid' secs a@(Asteroid (xPos, yPos) dir _ sp) = a { asteriodPos = (xPos - speed, yPos)}
  where speed = (secs * aS) * (10 * (sp / 100))

-- update the gamestate with the new bullet positions
moveBullets :: Float -> GameState -> GameState
moveBullets secs gstate = gstate{bullets = bullets'}
  where bullets' = map (moveBullet' secs) $ bullets gstate

-- handles the movement of an indifidual bullet
moveBullet' :: Float -> Bullet -> Bullet
moveBullet' secs b = Bullet (bulletPos b A.+ mulSV (secs * pS * 3) (bulletDir b) ) (bulletDir b)

--updates the gamestate with the new player for each key positions
movePlayer :: Float -> GameState -> GameState  -- if key is in keys gstate (meaning ispressed) 
movePlayer secs gstate = move secs strokes gstate
  where
    move :: Float -> [Key] -> GameState -> GameState
    move _  [] gst = gst
    move secs (s:ss) gst = move secs ss ex
      where
        ex :: GameState
        ex = movePlayer' secs s gst -- excecution of a single move
    strokes :: [Key]
    strokes = S.toList $ keys gstate


-- handles the movent of a player per individual key
movePlayer' :: Float ->  Key -> GameState -> GameState  -- if key is in keys gstate (meaning ispressed) 
movePlayer' secs key gstate | key == Char 'w' = gstate{ player1 = movePlayer'' UpDir secs (player1 gstate) }
                            | key == Char 's' = gstate{ player1 = movePlayer'' DownDir secs (player1 gstate) }
                            | key == Char 'a' = gstate{ player1 = movePlayer'' LeftDir secs (player1 gstate) }
                            | key == Char 'd' = gstate{ player1 = movePlayer'' RightDir secs (player1 gstate) }
                            | key == SpecialKey KeyUp    = gstate{ player2 = movePlayer'' UpDir secs (player2 gstate) }
                            | key == SpecialKey KeyDown  = gstate{ player2 = movePlayer'' DownDir secs (player2 gstate) }
                            | key == SpecialKey KeyLeft  = gstate{ player2 = movePlayer'' LeftDir secs (player2 gstate) }
                            | key == SpecialKey KeyRight = gstate{ player2 = movePlayer'' RightDir secs (player2 gstate) }
                            | otherwise = gstate

-- gives  the updated player after being moved into the given direction
movePlayer'' :: MoveDirection -> Float -> Player -> Player
movePlayer'' UpDir eTime p@(Player _ pos@(xPos,yPos) dir@(xDir, yDir) _) = p{playerPos  = (xPos + (xDir * pS * eTime), yPos + (yDir * pS * eTime))}
movePlayer'' DownDir eTime p@(Player _ pos@(xPos,yPos) dir@(xDir, yDir) _) = p{playerPos  = (xPos - (xDir * pS * eTime), yPos - (yDir * pS * eTime))}
movePlayer'' LeftDir eTime p@(Player _ (x,y) dir _) =  p{playerDir = movePlayerDirection LeftDir eTime dir}
movePlayer'' RightDir eTime p@(Player _ (x,y) dir _) = p{playerDir = movePlayerDirection RightDir eTime dir}

--gives the updated direction of the player
movePlayerDirection :: MoveDirection -> Float -> Direction -> Direction
movePlayerDirection LeftDir eTime dir@(x,y) =  movePlayerDirection' LeftDir (getPlayerDirection dir) eTime dir
movePlayerDirection RightDir eTime dir@(x,y) =  movePlayerDirection' RightDir (getPlayerDirection dir) eTime dir

thresHold :: Float -> Bool
thresHold x = x >= 1 || x < -1

movePlayerDirection' :: MoveDirection -> MoveDirection -> Float -> Direction -> Direction -- toDirection -> isInDirection -> eTime -> Direction
-- move right
movePlayerDirection' RightDir UpDir etime dir@(x,y) =
  let x' = x + dS * etime in if thresHold x' then (1 , y - dS * etime) else (x' ,y)
movePlayerDirection' RightDir DownDir etime dir@(x,y) =
  let x' = x - dS * etime in if thresHold x' then (-1, y + dS * etime) else (x' ,y)
movePlayerDirection' RightDir LeftDir etime dir@(x,y) =
  let y' = y + dS * etime in if thresHold y' then (x + dS * etime, 1) else (x ,y')
movePlayerDirection' RightDir RightDir etime dir@(x,y) =
  let y' = y - dS * etime in if thresHold y' then (x - dS * etime, -1) else (x ,y')
-- move left
movePlayerDirection' LeftDir UpDir etime dir@(x,y) =
  let x' = x - dS * etime in if thresHold x' then (-1 , y - dS * etime) else (x' ,y)
movePlayerDirection' LeftDir DownDir etime dir@(x,y) =
  let x' = x + dS * etime in if thresHold x' then (1,  y + dS * etime) else (x' ,y)
movePlayerDirection' LeftDir LeftDir etime dir@(x,y) =
  let y' = y - dS * etime in if thresHold y' then (x + dS * etime, -1) else (x ,y')
movePlayerDirection' LeftDir RightDir etime dir@(x,y) =
  let y' = y + dS * etime in if thresHold y' then (x - dS * etime, 1) else (x ,y')

getPlayerDirection :: Direction -> MoveDirection -- in what direction is it currently
getPlayerDirection dir@(x,y) | x <? (-1,1) && y == 1  = UpDir  -- up
                             | x <? (-1,1) && y == -1 = DownDir-- down
                             | y <? (-1,1) && x == -1 = LeftDir -- left
                             | y <? (-1,1) && x == 1  = RightDir -- right
                             | otherwise = RightDir