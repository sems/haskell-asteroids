-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case currentState gstate of
  Main        -> color green (text "Main")
  Playing     -> playmode gstate
  GameOver    -> color green (text "Gameover")
  Pause       -> color green (text "Pause")
  Leaderboard -> color green (text "Leaderboard")
  Choose      -> color green (text "choose")

playmode :: GameState -> Picture -- for test purpose 
playmode (GameState _ _ (Player 0 _ _ _) _ _) = color green (text "Single")
playmode _  = color green (text "coop")