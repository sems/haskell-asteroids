{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Model
    ( initialState,
      Asteroid(Asteroid),
      ScoreEntry(ScoreEntry, name, score),
      GameMode(Coop, SinglePlayer),
      GameState(GameState, asteroids, keys, currentState, player1, player2, playerName, collision),
      Player(Player, playerPos, time, lives),
      State(GameOver, Leaderboard, Pause, Choose, Main, Playing,GetName), asteriodPos, Direction, playerDir, Time, Position )
import View(playerPath, getScore)

import qualified Data.Aeson as Ae


import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeyEsc, KeyDown, KeyLeft, KeyRight, KeyUp, KeyBackspace, KeyEnter),
      Event(EventKey) )
import System.Random ( getStdRandom, Random(randomR) )

import Data.Set ( member )
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Constants ( pS, aS, baseSize, dS, (<?), MoveDirection (RightDir, LeftDir, DownDir) )
import Text.Printf (printf)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Exit (exitSuccess)
import Graphics.Gloss.Geometry.Line(closestPointOnLine)
import Graphics.Gloss.Data.Vector (dotV, angleVV, argV)
import qualified Graphics.Gloss.Data.Point.Arithmetic  as A ((-))
import qualified Data.ByteString.Lazy as B
import Movement (moveAsteroids, movePlayer)
import Data.Aeson.Types (Value(Bool))


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState Playing (Player 0 _ _ _) (Player 0 _ _ _) _ _ _ _ _) = insertScore gstate >> return gstate{currentState = GameOver}
step secs gstate@(GameState Playing _ _ _ _ _ _ _) = log' $ spawnAsteroid $ handleCollision  $ handleTime secs $ movePlayer secs $ moveAsteroids secs $ handleCollision' secs gstate
step _ gstate = return gstate

log' :: IO GameState -> IO GameState
log' gstate = do
  gstate'@(GameState _ p1 p2 _ _ _ _ col) <- gstate
  -- putStr "x:"
  -- putStr $ show $ getX p1
  -- putStr " y:"
  -- print (getY p1)
  -- print (argV (-1,-1))
  gstate
  where
    getX p@(Player _ pos dir@(x,y) _) = x
    getY p@(Player _ pos dir@(x,y) _) = y

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@(GameState GetName _ _ _ _ _ _ _) = return $ getName e gstate
input e gstate = handleExit e $ foldr (\f -> f e) gstate [testEvent, stateFlow, handleInput]

handleExit :: Event -> GameState -> IO GameState --closes the program when pressing esc in main state 
handleExit  (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Main _ _ _ _ _ _ _) = exitSuccess
handleExit _ gstate = return gstate

insertScore :: GameState -> IO ()
insertScore g | time (player2 g) == 0 =   getScore SinglePlayer >>= B.writeFile "SingleBoard.json" . Ae.encode . (entry :)
              | otherwise = getScore Coop >>= B.writeFile "CoopBoard.json" . Ae.encode  .(entry :)
  where entry =  ScoreEntry (playerName g) newScore
        newScore = round ( time (player2 g) + time (player1 g))


getName :: Event -> GameState -> GameState
getName (EventKey (SpecialKey KeyEnter) _ _ _) g = g{currentState = Choose}
getName (EventKey (SpecialKey KeyLeft) Down _ _) g | playerName g == "" = g
                                                   | otherwise = g{playerName = take (length (playerName g) - 1) (playerName g) }
getName (EventKey (Char c) Down _ _) g = g{playerName = playerName g ++ [c]}
getName _ g = g

-- force certain states within the game in order to test specific functions
testEvent :: Event -> GameState -> GameState
testEvent (EventKey (Char 'f') _ _ _) g@(GameState Playing _ _ _ _ _ _ _) = g {player2 = (player2 g) {lives = 0}} -- kill player 2
testEvent (EventKey (Char 'g') _ _ _) g@(GameState Playing _ _ _ _ _ _ _) = g {player1 = (player1 g) {lives = 0}} -- kill player 1
testEvent _ g = g

stateFlow :: Event -> GameState -> GameState -- flow between different  states (eventKey will probably be replaced with mouseinput)
stateFlow (EventKey (Char 'n') _ _ _) gstate@(GameState Main _ _ _ _ _ _ _) = initialState {currentState = GetName}  -- new game
stateFlow (EventKey (Char 'c') _ _ _) gstate@(GameState Main _ _ _ _ _ _ _) = gstate {currentState = Playing}      -- continue game
stateFlow (EventKey (Char '1') _ _ _) gstate@(GameState Choose _ _ _ _ _ _ _) = gstate {currentState = Playing, player2 = (player2 gstate){lives = 0}} -- singeplayer
stateFlow (EventKey (Char '2') _ _ _) gstate@(GameState Choose _ _ _ _ _ _ _) = gstate {currentState = Playing } -- coop
stateFlow (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Playing _ _ _ _ _ _ _) = gstate {currentState = Pause} --pause game
stateFlow (EventKey (Char 'c') _ _ _) gstate@(GameState Pause _ _ _ _ _ _ _) = gstate {currentState = Playing} -- contine game
stateFlow (EventKey (Char 'm') _ _ _) gstate@(GameState Pause _ _ _ _ _ _ _) = gstate {currentState = Main}  -- back to main menu (without losing progress)
stateFlow (EventKey (Char 'l') _ _ _) gstate@(GameState Main _ _ _ _ _ _ _) = gstate {currentState = Leaderboard} -- view leaderboard
stateFlow (EventKey (Char 'm') _ _ _) gstate@(GameState Leaderboard _ _ _ _ _ _ _) = gstate {currentState = Main} -- back to main menu 
stateFlow (EventKey (Char 'l') _ _ _) gstate@(GameState GameOver _ _ _ _ _ _ _) = initialState{currentState = Leaderboard}
stateFlow (EventKey (Char 'm') _ _ _) gstate@(GameState GameOver _ _ _ _ _ _ _) = initialState
stateFlow _ gstate = gstate


handleTime :: Float -> GameState -> GameState -- updates time for each player while in playing state if player is alive (when both players are alive their time are the same so the old time for player1 can be reused for player 2)
handleTime elapsedTime gstate@(GameState Playing p1@(Player _ _ _ oldTime) (Player 0 _ _ _) _ _ _ _ _) = gstate{player1 = p1{time = oldTime + elapsedTime}}
handleTime elapsedTime gstate@(GameState Playing (Player 0 _ _ _) p2@(Player _ _ _ oldTime) _ _ _ _ _) = gstate{player2 = p2{time = oldTime + elapsedTime}}
handleTime elapsedTime gstate@(GameState Playing p1@(Player _ _ _ oldTime) p2 _ _ _ _ _) =  gstate{player1 = p1{time = oldTime + elapsedTime}, player2 = p2{time = oldTime + elapsedTime} }
handleTime _ gstate = gstate


handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate = gstate { keys = S.delete k (keys gstate)}
handleInput _ world = world -- Ignore non-keypresses for simplicity

gameTime :: GameState -> Int
gameTime (GameState Playing (Player 0 _ _ _) (Player _ _ _ time) _ _ _ _ _) = round time
gameTime (GameState Playing (Player _ _ _ time) _ _ _ _ _ _) = round time
gameTime _ = 0

spawnAsteroid :: GameState -> IO GameState
spawnAsteroid gstate@(GameState _ _ _ astr _ _ _ _) = do
  let time = gameTime gstate
  newAstr <- newAsteroid
  if time `mod` 5 == 0 then return $ gstate{asteroids = astr ++ [newAstr] } else return gstate

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

--collision stuff
checkCollision :: Asteroid -> Player -> Bool
checkCollision _ (Player 0 _ _ _) = False
checkCollision (Asteroid pos@(ax,ay) _ s _) pl = doesCollide $ map (getdistance . getclosest pos) (getsegments $ playerPath pl)
  where getsegments[x,y,z] = [(x,y),(y,z),(x,z)]  -- distibute path into line segments
        getclosest p (x,y) | dotV xy yp > 0 = y    -- get from each line segemnt the closest point to mid asteroid (here x,y aree two points and not coordinates)
                           | dotV xy xp < 0 = x       --
                           | otherwise = closestPointOnLine x y p -- used function only looks for the closest point on a infinate line so only used if said closest point of 
          where xy = x A.- y                                         -- the infinate line is also within segment (which is the case when the needed point isnt either x or y itself)
                xp = x A.- p
                yp = y A.- p
        getdistance (x,y) = sqrt ((x - ax)*(x-ax)+ (y-ay)*(y-ay)) -- get distance between point and mid asteroid
        doesCollide :: (Ord a, Num a) => [a] -> Bool
        doesCollide [] = False
        doesCollide (x:xs) | x <= fromIntegral(s* baseSize) = True -- if distance is smaller than the size of asteroid it intersects
                           | otherwise = doesCollide xs

handleCollision :: GameState -> GameState
handleCollision gstate@(GameState _ p1 p2 astrs _ _ _ col) = loseLife (collisionWith astrs [] Nothing)
  where
    collisionWith :: [Asteroid] -> [Asteroid] -> Maybe Player -> (Maybe Asteroid, [Asteroid], Maybe Player)
    collisionWith [] ys may = (Nothing, ys, may)
    collisionWith (x:xs) ys may | checkCollision x p1 = (Just x, xs ++ ys, Just p1)
                                | checkCollision x p2 = (Just x, xs ++ ys, Just p2)
                                | otherwise = collisionWith xs (x:ys) may
    loseLife :: (Maybe Asteroid, [Asteroid], Maybe Player) -> GameState
    loseLife (colAs, as, may) = case colAs of
      Nothing -> gstate
      Just colAsteroid@(Asteroid pos _ _ _) -> case may of
        Nothing -> gstate
        Just player | player == p1 -> gstate{player1 = p1{lives = lives p1 - 1}, asteroids = as, collision = (pos, 1.0) : col}
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