{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
    ( GameMode(..),
      State(GetName, Choose, Pause, GameOver, Playing, Main,
            Leaderboard),
      ScoreEntry(ScoreEntry),
      Asteroid(Asteroid),
      GameState(GameState, collision, playerName, currentState, bullets),
      Player(Player),
      Bullet(Bullet, bulletPos, bulletDir) )
import Constants ( baseSize, (<?), eS )
import Graphics.Gloss.Data.Vector ( rotateV, angleVV, argV, mulSV )
import qualified Graphics.Gloss.Data.Point.Arithmetic  as A ((+))
import Graphics.Gloss.Geometry.Angle (degToRad)
import Data.List(sortBy)
import qualified Data.Aeson as Ae

instance Ae.ToJSON ScoreEntry where
    toEncoding = Ae.genericToEncoding Ae.defaultOptions

instance Ae.FromJSON ScoreEntry

view :: GameState -> IO Picture
view (GameState Leaderboard _ _ _ _ _ _ _) = mconcat [showLeaderboard SinglePlayer (-740), showLeaderboard Coop 0]
view g = return $ viewPure g

viewPure :: GameState -> Picture
viewPure gstate = case currentState gstate of -- temp indications for states
  Main        -> color green (text "Main")
  Playing     -> pictures (drawExplosions gstate ++ drawPlaying gstate ++ drawAsteroids gstate)
  GameOver    -> pictures $ drawGameOver gstate
  Pause       -> color green (text "Pause")
  Leaderboard -> color green (text "Leaderboard")
  Choose      -> color green (text "choose")
  GetName     -> color green (text $ playerName gstate)

drawGameOver :: GameState -> [Picture]
drawGameOver gstate@(GameState _ (Player _ _ _ time1) (Player _ _ _ time2) _ _ _ _ _) =
  [color green (text "Gameover"), translate 0 (-100) $ color green (text $ show score), translate 0 (-200) $ color green (text $ show mode)]
   where score = round (time1 + time2)
         mode  | time2 == 0 = SinglePlayer
               | otherwise = Coop

drawPlaying :: GameState -> [Picture]
drawPlaying gstate@(GameState _ p1 p2 _ _ _ _ _)  = drawBullets gstate : drawScore (-490,150) p1 : drawScore (0,150) p2 : drawPlayers gstate
  where drawScore _ (Player _ _ _ 0) = blank
        drawScore (x,y) (Player lives _ _ time) = translate x y $ color white $ text ("l:" ++ show lives ++ " s:" ++ show (round time))

drawPlayers :: GameState -> [Picture]
drawPlayers (GameState _ p1 p2 _ _ _ _ _) = [drawPlayer p1 blue , drawPlayer p2 yellow ]
  where drawPlayer (Player 0 _ _ _) _ = blank
        drawPlayer player col = color col $ polygon $ playerPath player

playerPath :: Player -> Path
playerPath (Player _ (x,y) dir _) = map (\(a,b) -> (a + x, b + y)) $ playerPath' dir

playerPath' :: Vector -> [Point]
playerPath' dir = map (rotateV (argV dir)) [(0, 10),(0,-10) ,(30, 0)]

drawAsteroids :: GameState -> [Picture]
drawAsteroids (GameState _ _ _ [] _ _ _ _) = [blank]
drawAsteroids (GameState _ _ _ astr _ _ _ _) = map drawAsteroid astr
  where
    drawAsteroid (Asteroid (x,y) dir siz sp) = translate x y $ color white $ circle  (fromIntegral (siz * baseSize))

drawExplosions :: GameState -> [Picture]
drawExplosions (GameState _ _ _ _ _ _ _ []) = [blank]
drawExplosions gstate@(GameState _ _ _ _ _ _ _ (col@((x,y), time):cols)) = explosion x y : drawExplosions gstate{collision = cols}
  where
    -- Creates a single 'boom' relative to the given location of the collision.
    explosion x y = translate x y $ color explosionColor $ thickCircle (eS * snd col) 2
    -- The color needs to be calculated individually since it can be faded out in this way.
    explosionColor = makeColor 251 251 251 ((time + 1) / 2)

drawBullets :: GameState -> Picture
drawBullets gstate = pictures $ map draw  $ bullets gstate
  where draw b = color white $ line $ bulletPath b

bulletPath :: Bullet -> Path
bulletPath b = [bulletPos b, bulletPos b A.+ mulSV 10 (bulletDir b)]

getScore :: GameMode -> IO [ScoreEntry]
getScore m = list <$> mlist m
  where mlist SinglePlayer = Ae.decodeFileStrict "SingleBoard.json"
        mlist Coop = Ae.decodeFileStrict "CoopBoard.json"
        list (Just a) = a
        list Nothing = []

showLeaderboard :: GameMode -> Float -> IO Picture
showLeaderboard g f =  drawBoard 300 <$> board (lSort <$> getScore g)
  where lSort = sortBy (\(ScoreEntry _ a) (ScoreEntry _ b) -> compare b a)
        board = fmap (foldl addEntry [title g] ) 
        addEntry list (ScoreEntry n s)  = list ++ ["Name:" ++ n ++ " Score:" ++ show s  ]
        drawBoard i [] = translate f i $ scale 0.45 0.45 $ color green (text "~~~~~~~~~~~~~~~~~~")
        drawBoard i (p:ps) = pictures[translate f i (  scale 0.45 0.45 $ color green (text p)) , drawBoard (i-70) ps]
        title SinglePlayer = "SinglePlayer"
        title Coop = "Coop"
