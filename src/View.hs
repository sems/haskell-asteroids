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
      GameState(GameState,currentState, playerName),
      Player(Player),
      ScoreEntry(ScoreEntry),
      Button(Button),bContinueM,bContinueP,bCoop,bLeaderG,bLeaderM,bLeaderP,bMainG,bMainL,bMainP,bNewGame,bSingle )
import DrawGameObject(drawPlayers,drawAsteroids,drawExplosions,drawBullets)
import Buttons(isPlaying)
import Constants (bHeight)
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
  Main        -> drawMain gstate
  Playing     -> pictures (drawExplosions gstate ++ drawPlaying gstate ++ drawAsteroids gstate)
  GameOver    -> drawGameOver gstate
  Pause       -> drawPause
  Choose      -> drawChoose
  GetName     -> drawGetName gstate


drawPlaying :: GameState -> [Picture]
drawPlaying gstate@(GameState _ p1 p2 _ _ _ _ _)  = drawBullets gstate : drawScore (-490,150) p1 : drawScore (0,150) p2 : drawPlayers gstate
  where drawScore _ (Player _ _ _ 0) = blank
        drawScore (x,y) (Player lives _ _ time) = translate x y $ color white $ text ("l:" ++ show lives ++ " s:" ++ show (round time))

drawGameOver :: GameState -> Picture
drawGameOver gstate@(GameState _ (Player _ _ _ time1) (Player _ _ _ time2) _ _ _ _ _) =
  pictures [color white (translate (-300) 250 $ text "Gameover"), translate (-70) 100 $ color white (text $ show score), drawButton bLeaderG, drawButton bMainG]
   where score = round (time1 + time2)

drawMain :: GameState -> Picture
drawMain gstate =  pictures[drawContinue, drawButton bNewGame, drawButton bLeaderM,translate (-300) 230 $ color white (text "Asteroids")]
  where drawContinue | isPlaying gstate = drawButton bContinueM
                     | otherwise = blank --when there hasn't started a game yet the continue button will not be visible as it's not usable

drawPause :: Picture
drawPause = pictures[translate (-200) 250 $ color white (text "Pause"), drawButton bContinueP, drawButton bMainP, drawButton bLeaderP]

drawChoose :: Picture
drawChoose = pictures[drawButton bSingle, drawButton bCoop, translate (-350) 220 $ color white $ text "Gamemode"]

drawGetName :: GameState -> Picture
drawGetName gstate = pictures[translate (-200) 0 $ color white $ text $ playerName gstate ,translate (-400) 250 $ color white $ text "Enter Name"]

-- draws button as a white box with a text inside of it
drawButton :: Button -> Picture
drawButton (Button width (x,y) btext) = translate x y $ color white $ pictures [translate 0 3 $ text btext, line [(0,0),(0,bHeight),(width,bHeight),(width,0),(0,0)]]

--
getScore :: GameMode -> IO [ScoreEntry]
getScore m = list <$> mlist m
  where mlist SinglePlayer = Ae.decodeFileStrict "SingleBoard.json"
        mlist Coop = Ae.decodeFileStrict "CoopBoard.json"
        list (Just a) = a
        list Nothing = []

showLeaderboard :: GameMode -> Float -> IO Picture
showLeaderboard g f =  drawBoard 300 <$> board (getTop <$> getScore g)
  where getTop =(take 6) .sortBy (\(ScoreEntry _ a) (ScoreEntry _ b) -> compare b a) --after sorting only the top 6 is returned so it'll fit on the screen
        board = fmap (foldl addEntry [title g] ) 
        addEntry list (ScoreEntry n s)  = list ++ ["Name:" ++ n ++ " Score:" ++ show s  ]
        drawBoard i [] = translate f i $ scale 0.45 0.45 $ color white (text "~~~~~~~~~~~~~~~~~~")
        drawBoard i (p:ps) = pictures[translate f i (  scale 0.45 0.45 $ color white (text p)) , drawBoard (i-70) ps]
        title SinglePlayer = "SinglePlayer"
        title Coop = "Coop"
