{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


import Model
    ( GameMode(..),
      State(GetName, Choose, Pause, GameOver, Playing, Main,
            Leaderboard),
      ScoreEntry(ScoreEntry),
      Asteroid(Asteroid),
      GameState(GameState, collision, playerName, currentState, bullets),
      Player(Player),
      Bullet(Bullet, bulletPos, bulletDir),
      Button(Button),bContinueM,bContinueP,bCoop,bLeaderG,bLeaderM,bLeaderP,bMainG,bMainL,bMainP,bNewGame,bSingle )
import Constants ( baseSize, (<?), eS ,bHeight)
import Graphics.Gloss.Data.Vector ( rotateV, angleVV, argV, mulSV )
import qualified Graphics.Gloss.Data.Point.Arithmetic  as A ((+))
import Graphics.Gloss.Geometry.Angle (degToRad)
import Data.List(sortBy)
import qualified Data.Aeson as Ae

instance Ae.ToJSON ScoreEntry where
    toEncoding = Ae.genericToEncoding Ae.defaultOptions

instance Ae.FromJSON ScoreEntry

view :: GameState -> IO Picture
view (GameState Leaderboard _ _ _ _ _ _ _) = mconcat [return(drawButton bMainL),showLeaderboard SinglePlayer (-740), showLeaderboard Coop 0]
view g = return $ viewPure g

viewPure :: GameState -> Picture
viewPure gstate = case currentState gstate of -- temp indications for states
  Main        -> drawMain
  Playing     -> pictures (drawExplosions gstate ++ drawPlaying gstate ++ drawAsteroids gstate)
  GameOver    -> drawGameOver gstate
  Pause       -> drawPause
  Choose      -> drawChoose
  GetName     -> drawGetName gstate


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


drawGameOver :: GameState -> Picture
drawGameOver gstate@(GameState _ (Player _ _ _ time1) (Player _ _ _ time2) _ _ _ _ _) =
  pictures [color white (translate (-300) 250 $ text "Gameover"), translate (-70) 100 $ color white (text $ show score), drawButton bLeaderG, drawButton bMainG]
   where score = round (time1 + time2)

drawMain :: Picture
drawMain = pictures[drawButton bContinueM, drawButton bNewGame, drawButton bLeaderM,translate (-300) 230 $ color white (text "Asteroids")]

drawPause :: Picture
drawPause = pictures[translate (-150) 250 $ color white (text "Pause"), drawButton bContinueP, drawButton bMainP, drawButton bLeaderP]

drawChoose :: Picture
drawChoose = pictures[drawButton bSingle, drawButton bCoop, translate (-300) 220 $ color white $ text "Gamemode"]

drawGetName :: GameState -> Picture
drawGetName gstate = pictures[translate (-200) 0 $ color white $ text $ playerName gstate ,translate (-300) 250 $ color white $ text "Enter Name"]

-- draw button
drawButton :: Button -> Picture
drawButton (Button width (x,y) btext) = translate x y $ color white $ pictures [translate 0 3 $ text btext, line [(0,0),(0,bHeight),(width,bHeight),(width,0),(0,0)]]

