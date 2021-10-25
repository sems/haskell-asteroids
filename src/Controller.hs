module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate 
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = 
    do 
      randomNumber <- randomIO
      let newNumber = abs randomNumber `mod` 10
      return $ GameState (ShowANumber newNumber) 0 
  | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate

