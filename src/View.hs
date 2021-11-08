{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Constants ( baseSize, (<?) )
import Graphics.Gloss.Data.Vector ( rotateV, angleVV, argV)
import Graphics.Gloss.Geometry.Angle (degToRad)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case currentState gstate of -- temp indications for states
  Main        -> color green (text "Main")
  Playing     -> pictures (drawPlaying gstate ++ drawAsteroids gstate)
  GameOver    -> pictures $ drawGameOver gstate
  Pause       -> color green (text "Pause")
  Leaderboard -> color green (text "Leaderboard")
  Choose      -> color green (text "choose")

drawGameOver :: GameState -> [Picture]
drawGameOver gstate@(GameState _ (Player _ _ _ time1) (Player _ _ _ time2) _ _ _ _) =
  [color green (text "Gameover"), translate 0 (-100) $ color green (text $ show score), translate 0 (-200) $ color green (text $ show mode)]
   where score = round (time1 + time2)
         mode  | time2 == 0 = SinglePlayer
               | otherwise = Coop

drawPlaying :: GameState -> [Picture]
drawPlaying gstate@(GameState _ p1 p2 _ _ _ _)  =  drawScore (-490,150) p1 : drawScore (0,150) p2 : drawPlayers gstate
  where drawScore _ (Player _ _ _ 0) = blank
        drawScore (x,y) (Player lives _ _ time) = translate x y $ color white $ text ("l:" ++ show lives ++ " s:" ++ show (round time))

drawPlayers :: GameState -> [Picture]
drawPlayers (GameState _ p1 p2 _ _ _ _) = [drawPlayer p1 blue , drawPlayer p2 yellow ]
  where drawPlayer (Player 0 _ _ _) _ = blank
        drawPlayer player col = color col $ polygon $ playerPath player

playerPath :: Player -> Path
playerPath (Player _ (x,y) dir _) = map (\(a,b) -> (a + x, b + y)) $ playerPath' dir

playerPath' :: Vector -> [Point]
playerPath' dir = map (rotateV (argV dir)) [(0, 10),(0,-10) ,(30, 0)]

drawAsteroids :: GameState -> [Picture]
drawAsteroids (GameState _ _ _ [] _ _ _) = [blank]
drawAsteroids (GameState _ _ _ astr _ _ _) = map drawAsteroid astr
  where
    drawAsteroid (Asteroid (x,y) dir siz sp) = translate x y $ color white $ circle  (fromIntegral (siz * baseSize))
