{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Model
    ( initialState,
      GameState(GameState, keys, currentState, player1, player2, playerName),
      Player(Player, playerPos, time, lives),
      State(Playing, Main, GetName,Choose,GameOver),
      asteriodPos, Direction, playerDir , Time, Position)
import Collision(handleBulletCollision, handleCollision, handleCollision')
import Buttons(handleButtons)
import Movement (moveAsteroids, movePlayer,moveBullets)
import HighscoreInteraction(insertScore)
import GameObjects( handleShot, spawnAsteroid)

import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeyEsc, KeyLeft,KeyEnter),
      Event(EventKey) )

import System.Exit (exitSuccess)
import qualified Data.Set as S

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState Playing (Player 0 _ _ _) (Player 0 _ _ _) _ _ _ _ _) = insertScore gstate >> return gstate{currentState = GameOver}
step secs gstate@(GameState Playing _ _ _ _ _ _ _) =  spawnAsteroid $ handleBulletCollision $ handleCollision  $ foldr (\f -> f secs) gstate [handleTime,movePlayer,moveAsteroids, moveBullets, handleCollision']  
step _ gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@(GameState GetName _ _ _ _ _ _ _) = return $ getName e gstate
input e gstate = handleExit e $ foldr (\f -> f e) gstate [  handleInput, handleShot,handleButtons]

-- keeps track of what keys are currently pressed 
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate = gstate { keys = S.delete k (keys gstate)}
handleInput _ world = world -- Ignore non-keypresses for simplicity

-- closes the program when pressing esc in main state 
handleExit :: Event -> GameState -> IO GameState 
handleExit  (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Main _ _ _ _ _ _ _) = exitSuccess
handleExit _ gstate = return gstate

-- updates time for each player while in playing state if player is alive (when both players are alive their time are the same so the old time for player1 can be reused for player 2)
handleTime :: Float -> GameState -> GameState 
handleTime elapsedTime gstate@(GameState Playing p1@(Player _ _ _ oldTime) (Player 0 _ _ _) _ _ _ _ _) = gstate{player1 = p1{time = oldTime + elapsedTime}}
handleTime elapsedTime gstate@(GameState Playing (Player 0 _ _ _) p2@(Player _ _ _ oldTime) _ _ _ _ _) = gstate{player2 = p2{time = oldTime + elapsedTime}}
handleTime elapsedTime gstate@(GameState Playing p1@(Player _ _ _ oldTime) p2 _ _ _ _ _) =  gstate{player1 = p1{time = oldTime + elapsedTime}, player2 = p2{time = oldTime + elapsedTime} }
handleTime _ gstate = gstate

-- allows the player to enter their name
getName :: Event -> GameState -> GameState
getName (EventKey (SpecialKey KeyEnter) _ _ _) g = g{currentState = Choose}
getName (EventKey (SpecialKey KeyLeft) Down _ _) g | playerName g == "" = g
                                                   | otherwise = g{playerName = take (length (playerName g) - 1) (playerName g) }
getName (EventKey (Char c) Down _ _) g = g{playerName = playerName g ++ [c]}
getName _ g = g
