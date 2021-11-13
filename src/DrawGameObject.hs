module DrawGameObject where -- draw functions for the several objects within the game

import Model
  (GameState(GameState, bullets, collision),
  Player(Player),
  Asteroid(Asteroid),
  Bullet(bulletDir, bulletPos))
import Constants(baseSize, (<?), eS)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector ( rotateV, angleVV, argV, mulSV )

import qualified Graphics.Gloss.Data.Point.Arithmetic  as A ((+))


drawPlayers :: GameState -> [Picture]
drawPlayers (GameState _ p1 p2 _ _ _ _ _) = [drawPlayer p1 blue , drawPlayer p2 yellow ]
  where drawPlayer (Player 0 _ _ _) _ = blank
        drawPlayer player col = color col $ polygon $ playerPath player

drawAsteroids :: GameState -> [Picture]
drawAsteroids (GameState _ _ _ [] _ _ _ _) = [blank]
drawAsteroids (GameState _ _ _ astr _ _ _ _) = map drawAsteroid astr
  where
    drawAsteroid (Asteroid (x,y) dir siz sp) = translate x y $ color white $ circle  (fromIntegral (siz * baseSize))

drawExplosions :: GameState -> [Picture]
drawExplosions (GameState _ _ _ _ _ _ _ []) = [blank]
drawExplosions gstate@(GameState _ _ _ _ _ _ _ (col@((x,y), time):cols)) = explosion x y : drawExplosions gstate{collision = cols}
  where
    -- Creates a single 'boom' relative to the given location of the collision.
    explosion x y = translate x y $ color explosionColor $ thickCircle (eS * snd col) 2
    -- The color needs to be calculated individually since it can be faded out in this way.
    explosionColor = makeColor 251 251 251 ((time + 1) / 2)

drawBullets :: GameState -> Picture
drawBullets gstate = pictures $ map draw  $ bullets gstate
  where draw b = color white $ line $ bulletPath b

--
playerPath :: Player -> Path
playerPath (Player _ (x,y) dir _) = map (\(a,b) -> (a + x, b + y)) $ playerPath' dir

playerPath' :: Vector -> [Point]
playerPath' dir = map (rotateV (argV dir)) [(0, 10),(0,-10) ,(30, 0)]

bulletPath :: Bullet -> Path
bulletPath b = [bulletPos b, bulletPos b A.+ mulSV 10 (bulletDir b)]
