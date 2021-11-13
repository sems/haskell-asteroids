module GameObjects where -- handles the creation of asteroids and bullets

import Model
    ( GameState(GameState, asteroids, bullets),
    State(Playing),
    Player(Player, playerPos, playerDir, lives),
    Asteroid(Asteroid),
      Bullet(Bullet, bulletPos, bulletDir),)

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

import System.Random (getStdRandom, Random(randomR))

-- generates a bullet that moves into the directio that the player faces if the right button is pressed
handleShot :: Event -> GameState -> GameState
handleShot (EventKey (Char c) Down _ _ ) g@(GameState Playing p1 p2 _ _ _ _ _ ) | c == 'm' && lives p2 > 0 =  g{bullets = newBull p2 : bullets g}
                                                                                | c == 'f' && lives p1> 0 = g{bullets = newBull p1 : bullets g}
                                                                                |otherwise = g
  where newBull p = Bullet (playerPos p) (playerDir p)
handleShot _ g = g

-- adds new asteroids to the ganestate at an certain interval
spawnAsteroid :: GameState -> IO GameState
spawnAsteroid gstate@(GameState _ _ _ astr _ _ _ _) = do
  let time = gameTime gstate
  newAstr <- newAsteroid
  if time `mod` 5 == 0 then return $ gstate{asteroids = astr ++ [newAstr] } else return gstate

 -- returns the time the current game (not the program) has been running 
gameTime :: GameState -> Int
gameTime (GameState Playing (Player 0 _ _ _) (Player _ _ _ time) _ _ _ _ _) = round time
gameTime (GameState Playing (Player _ _ _ time) _ _ _ _ _ _) = round time
gameTime _ = 0

--generates a new asteroids with an random size , speed, and location
newAsteroid :: IO Asteroid
newAsteroid = do
  (widthX, heightY) <- getScreenSize
  xPos <- getStdRandom (randomR (0 + ((10 * widthX) `div` 100), widthX `div` 2))
  yPos <- getStdRandom (randomR (heightY * (-1), heightY `div` 2))
  xDir <- getStdRandom (randomR (0, widthX))
  yDir <- getStdRandom (randomR (0, heightY))
  astrSize <- getStdRandom (randomR (50, 1000))
  speed <- getStdRandom (randomR (50, 200))
  return $ Asteroid (realToFrac xPos, realToFrac yPos) (realToFrac xDir, realToFrac yDir) (astrSize `div` 100) (speed / 100)

