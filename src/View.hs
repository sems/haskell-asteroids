-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case currentState gstate of -- temp indications for states
  Main        -> color green (text "Main")
  Playing     -> pictures $ drawPlaying gstate
  GameOver    -> pictures $ drawGameOver gstate
  Pause       -> color green (text "Pause")
  Leaderboard -> color green (text "Leaderboard")
  Choose      -> color green (text "choose")

drawGameOver :: GameState -> [Picture]
drawGameOver gstate@(GameState _ (Player _ _ _ time1) (Player _ _ _ time2) _ _) = 
  color green (text "Gameover") : (translate 0 (-100) $ color green (text $ show score) ): (translate 0 (-200) $ color green (text $ show mode)) : []
   where score = round (time1 + time2)
         mode  | time2 == 0 = SinglePlayer
               | otherwise = Coop

drawPlaying :: GameState -> [Picture] 
drawPlaying gstate@(GameState _ p1 p2 _ _)  =  drawScore (-490,150) p1 : drawScore (0,150) p2 : drawPlayers gstate
  where drawScore _ (Player _ _ _ 0) = blank
        drawScore (x,y) (Player lives _ _ time) = translate x y $ color white $ text ("l:" ++ show lives ++ " s:" ++ show (round(time)))


drawPlayers :: GameState -> [Picture]
drawPlayers (GameState _ (Player lives1 pos1 _ _) (Player lives2 pos2 _ _) _ _) = [drawPlayer pos1 blue lives1 , drawPlayer pos2 yellow lives2]
  where drawPlayer _ _ 0 = blank
        drawPlayer (x,y) col _ = translate x y $ color col $ polygon [(0,0),(10,30),(20,0)]

 
  



