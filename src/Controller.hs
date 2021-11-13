{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Model
    ( initialState,
      Asteroid(Asteroid),
      ScoreEntry(ScoreEntry, name, score),
      GameMode(Coop, SinglePlayer),
      GameState(GameState, asteroids, keys, currentState, player1, player2, bullets, playerName,collision),
      Player(Player, playerPos, time, lives),
      Bullet(Bullet, bulletPos, bulletDir),
      State(GameOver, Leaderboard, Pause, Choose, Main, Playing,GetName), asteriodPos, Direction, playerDir , Time, Position)
import View(getScore)
import Collision(handleBulletCollision, handleCollision, handleCollision')
import Buttons(handleButtons)

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

import qualified Data.ByteString.Lazy as B
import Movement (moveAsteroids, movePlayer,moveBullets)
import Data.Aeson.Types (Value(Bool))


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState Playing (Player 0 _ _ _) (Player 0 _ _ _) _ _ _ _ _) = insertScore gstate >> return gstate{currentState = GameOver}
step secs gstate@(GameState Playing _ _ _ _ _ _ _) =  spawnAsteroid $ handleBulletCollision $ handleCollision  $ foldr (\f -> f secs) gstate [handleTime,movePlayer,moveAsteroids, moveBullets, handleCollision']  
step _ gstate = return gstate

-- | Handle user input

input :: Event -> GameState -> IO GameState
input e gstate@(GameState GetName _ _ _ _ _ _ _) = return $ getName e gstate
input e gstate = handleExit e $ foldr (\f -> f e) gstate [  handleInput, handleShot,handleButtons]


handleShot :: Event -> GameState -> GameState
handleShot (EventKey (Char 'm') Down _ _ ) g@(GameState Playing _ p _ _ _ _ _ ) | lives p == 0 = g
                                                                                | otherwise =  g{bullets = newBull : bullets g}
  where newBull = Bullet (playerPos p) (playerDir p)
handleShot (EventKey (Char 'f') Down _ _ ) g@(GameState Playing p _ _ _ _ _ _ ) | lives p == 0 = g
                                                                                |otherwise =  g{bullets = newBull : bullets g}
  where newBull = Bullet (playerPos p) (playerDir p)
handleShot _ g = g 


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

