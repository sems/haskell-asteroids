module Controller where

import Model
    ( initialState,
      Asteroid(Asteroid),
      GameState(GameState, asteroids, keys, currentState, player1, player2),
      Player(Player, playerPos, time, lives),
      State(GameOver, Leaderboard, Pause, Choose, Main, Playing), asteriodPos )
import View

import Graphics.Gloss ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeyEsc, KeyDown, KeyLeft, KeyRight, KeyUp),
      Event(EventKey) )
import System.Random ( getStdRandom, Random(randomR) )

import Data.Set ( member )
import qualified Data.Set as S
import Constants ( pS, aS, baseSize ) 
import Text.Printf (printf)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Exit ()
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as A

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState Playing _ _ _ _ _) = spawnAsteroid $ checkDeath $ handleCollision  $ handleTime secs $ movePlayer secs $ moveAsteroids secs gstate
step _ gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return $ foldr (\f -> f e) gstate [testEvent, stateFlow, handleInput]

-- force certain states within the game in order to test specific functions
testEvent :: Event -> GameState -> GameState
testEvent (EventKey (Char 'f') _ _ _) g@(GameState Playing _ _ _ _ _) = g {player2 = (player2 g) {lives = 0}} -- kill player 2
testEvent (EventKey (Char 'g') _ _ _) g@(GameState Playing _ _ _ _ _) = g {player1 = (player1 g) {lives = 0}} -- kill player 1
testEvent _ g = g

stateFlow :: Event -> GameState -> GameState -- flow between different  states (eventKey will probably be replaced with mouseinput)
stateFlow (EventKey (Char 'n') _ _ _) gstate@(GameState Main _ _ _ _ _) = initialState {currentState = Choose}  -- new game
stateFlow (EventKey (Char 'c') _ _ _) gstate@(GameState Main _ _ _ _ _) = gstate {currentState = Playing}      -- continue game
stateFlow (EventKey (Char '1') _ _ _) gstate@(GameState Choose _ _ _ _ _) = gstate {currentState = Playing, player2 = (player2 gstate){lives = 0}} -- singeplayer
stateFlow (EventKey (Char '2') _ _ _) gstate@(GameState Choose _ _ _ _ _) = gstate {currentState = Playing } -- coop
stateFlow (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Playing _ _ _ _ _) = gstate {currentState = Pause} --pause game
stateFlow (EventKey (Char 'c') _ _ _) gstate@(GameState Pause _ _ _ _ _) = gstate {currentState = Playing} -- contine game
stateFlow (EventKey (Char 'm') _ _ _) gstate@(GameState Pause _ _ _ _ _) = gstate {currentState = Main}  -- back to main menu (without losing progress)
stateFlow (EventKey (Char 'l') _ _ _) gstate@(GameState Main _ _ _ _ _) = gstate {currentState = Leaderboard} -- view leaderboard
stateFlow (EventKey (Char 'm') _ _ _) gstate@(GameState Leaderboard _ _ _ _ _) = gstate {currentState = Main} -- back to main menu 
stateFlow (EventKey (Char 'l') _ _ _) gstate@(GameState GameOver _ _ _ _ _) = initialState{currentState = Leaderboard}
stateFlow (EventKey (Char 'm') _ _ _) gstate@(GameState GameOver _ _ _ _ _) = initialState
stateFlow (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Main _ _ _ _ _ ) = sneakyExit 0 -- closes game 
stateFlow (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState Pause _ _ _ _ _ ) = sneakyExit 0
stateFlow _ gstate = gstate

sneakyExit :: Int -> GameState
sneakyExit 1 = initialState -- will closes the game because non-exhaustive pattern error (if it works it works :)


handleTime :: Float -> GameState -> GameState -- updates time for each player while in playing state if player is alive (when both players are alive their time are the same so the old time for player1 can be reused for player 2)
handleTime elapsedTime gstate@(GameState Playing p1@(Player _ _ _ oldTime) (Player 0 _ _ _) _ _ _) = gstate{player1 = p1{time = oldTime + elapsedTime}}
handleTime elapsedTime gstate@(GameState Playing (Player 0 _ _ _) p2@(Player _ _ _ oldTime) _ _ _) = gstate{player2 = p2{time = oldTime + elapsedTime}}
handleTime elapsedTime gstate@(GameState Playing p1@(Player _ _ _ oldTime) p2 _ _ _) =  gstate{player1 = p1{time = oldTime + elapsedTime}, player2 = p2{time = oldTime + elapsedTime} }
handleTime _ gstate = gstate

checkDeath :: GameState -> GameState
checkDeath gstate@(GameState _ (Player 0 _ _ _) (Player 0 _ _ _) _ _ _) = gstate{ currentState = GameOver}
checkDeath gstate = gstate

handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate = gstate { keys = S.delete k (keys gstate)}
handleInput _ world = world -- Ignore non-keypresses for simplicity

gameTime :: GameState -> Int
gameTime (GameState Playing (Player 0 _ _ _) (Player _ _ _ time) _ _ _) = round time
gameTime (GameState Playing (Player _ _ _ time) _ _ _ _) = round time
gameTime _ = 0

spawnAsteroid :: GameState -> IO GameState
spawnAsteroid gstate@(GameState _ _ _ astr _ _) = do
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

data MoveDirection = UpDir | DownDir | LeftDir | RightDir

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


-- movement
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


movePlayer'' :: MoveDirection -> Float -> Player -> Player
movePlayer'' UpDir eTime p@(Player _ (x,y) _ _) = p{playerPos    = (x, y + pS * eTime)}
movePlayer'' DownDir eTime p@(Player _ (x,y) _ _) = p{playerPos  = (x, y - pS * eTime)}
movePlayer'' LeftDir eTime p@(Player _ (x,y) _ _) = p{playerPos  = (x - pS * eTime, y)}
movePlayer'' RightDir eTime p@(Player _ (x,y) _ _) = p{playerPos = (x + pS * eTime, y)}

moveAsteroids :: Float -> GameState -> GameState
moveAsteroids secs gstate = gstate { asteroids = asteroids' }
  where 
    asteroids':: [Asteroid]
    asteroids' = map (moveAsteroid' secs) $ asteroids gstate

moveAsteroid' :: Float -> Asteroid -> Asteroid
moveAsteroid' secs a@(Asteroid (xPos, yPos) dir _ sp) = a { asteriodPos = (xPos - speed, yPos)}
  where speed = (secs * aS) * (10 * (sp / 100)) 

--collision stuff
checkCollision :: Asteroid -> Player -> Bool
checkCollision _ (Player 0 _ _ _) = False
checkCollision (Asteroid pos@(ax,ay) _ s _) pl = thing $ map getdistance $ map (getclosest pos) $ getsegments $ playerPath pl 
  where getsegments[x,y,z] = [(x,y),(y,z),(x,z)]  -- distibute path into line segments
        getclosest p (x,y) | dotV xy yp > 0 = y    -- get from each line segemnt the closest point to mid asteroid (here x,y aree two points and not coordinates)
                           | dotV xy xp < 0 = x       --
                           | otherwise = closestPointOnLine x y p -- used function only looks for the closest point on a infinate line so only used if said closest point of 
          where xy = x A.- y                                         -- the infinate line is also within segment (which is the case when the needed point isnt either x or y itself)
                xp = x A.- p
                yp = y A.- p
        getdistance (x,y) = sqrt ((x - ax)*(x-ax)+ (y-ay)*(y-ay)) -- get distance between point and mid asteroid
        thing [] = False 
        thing (x:xs) | x <= fromIntegral(s* baseSize) = True -- if distance is smaller than the size of asteroid it intersects
                     | otherwise = thing xs

handleCollision :: GameState -> GameState 
handleCollision gstate@(GameState _ p1 p2 astrs _ _) = otherthing (thing astrs [] Nothing)
  where thing [] ys may = (ys, may)
        thing (x:xs) ys may | checkCollision x p1 = ((xs ++ ys), Just p1)
                            | checkCollision x p2 = ((xs ++ ys), Just p2)
                            | otherwise = thing xs (x:ys) may
        otherthing (as, may) | may == Just p1 = gstate{player1 = p1{lives = lives p1 - 1},asteroids = as} -- sometimes I just don't know what to call functions okey (especially when I don't have the energy to think too much about it (might change later))
                             | may == Just p2 = gstate{player2 = p2{lives = lives p2 - 1},asteroids = as}
                             | otherwise = gstate

