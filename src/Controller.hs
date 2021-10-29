module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Data.Set
import qualified Data.Set as S
import Constants (screenWidth, screenHeigth)
import Model (Asteroid(Asteroid), GameState (GameState))
import Text.Printf (printf)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState Playing _ _ _ _ _) = spawnAsteroid $ checkDeath $ handleTime secs gstate
step _ gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return $ testEvent e $ stateFlow e gstate 

-- force certain states within the game in order to test specific functions
testEvent :: Event -> GameState -> GameState
testEvent (EventKey (Char 'd') _ _ _) g@(GameState Playing _ _ _ _ _) = g {player2 = (player2 g) {lives = 0}} -- kill player 2
testEvent (EventKey (Char 'f') _ _ _) g@(GameState Playing _ _ _ _ _) = g {player1 = (player1 g) {lives = 0}} -- kill player 1
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
-- movement
stateFlow (EventKey (Char 'w') _ _ _) gstate@(GameState Playing p1 _ _ _ _) = movePlayer p1 UpDir gstate
stateFlow _ gstate = gstate

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

getElapsedTime :: GameState -> Int 
getElapsedTime gstate@(GameState Playing p1@(Player _ _ _ t1) p2@(Player _ _ _ t2) _ _ _) = max (round t1) $ round t2
getElapsedTime gstate = 0

spawnAsteroid :: GameState -> IO GameState
spawnAsteroid gstate@(GameState _ _ _ astr _ _) = do
  let time = getElapsedTime gstate
  newAstr <- newAsteroid
  if time `mod` 5 == 0 then return $ gstate{asteroids = astr ++ [newAstr] } else return gstate

newAsteroid :: IO Asteroid
newAsteroid = do
  xPos <- getStdRandom (randomR (screenWidth `div` 2 , screenWidth))
  yPos <- getStdRandom (randomR (0, screenHeigth))
  xDir <- getStdRandom (randomR (0, screenWidth))
  yDir <- getStdRandom (randomR (0, screenHeigth))
  astrSize <- getStdRandom (randomR (50, 1000))
  speed <- getStdRandom (randomR (50, 200))
  return $ Asteroid (realToFrac xPos, realToFrac yPos) (realToFrac xDir, realToFrac yDir) (astrSize `div` 100) (speed / 100)

pureAsteroid :: Asteroid
pureAsteroid = Asteroid (realToFrac screenWidth/2, realToFrac screenHeigth/2) (0,0) 2 1

data MoveDirection = UpDir | DownDir | LeftDir | RightDir

movePlayer :: Player -> MoveDirection -> GameState -> GameState
movePlayer p@(Player liv post dir oldTime) d gstate = gstate{player1 = movePlayer' d p} 

movePlayer' :: MoveDirection -> Player -> Player 
movePlayer' UpDir p@(Player _ (x,y) _ _) = p{playerPos = (x,y+1)}
movePlayer' DownDir p@(Player _ (x,y) _ _) = p{playerPos = (x,y-1)}
movePlayer' LeftDir p@(Player _ (x,y) _ _) = p{playerPos = (x-1,y)}
movePlayer' RightDir p@(Player _ (x,y) _ _) = p{playerPos = (x+1,y)}
