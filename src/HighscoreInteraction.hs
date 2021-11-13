module HighscoreInteraction where

import Model
  (GameState(player1, player2, playerName),
  Player(time), 
  ScoreEntry(ScoreEntry),
  GameMode(SinglePlayer,Coop))

import Graphics.Gloss.Interface.IO.Game
    ( white, color, pictures, scale, text, translate, Picture )

import Data.List(sortBy)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

instance A.ToJSON ScoreEntry where
    toEncoding = A.genericToEncoding A.defaultOptions
    
instance A.FromJSON ScoreEntry

--adds the reached score to the corresponding json file togherer with the given name
insertScore :: GameState -> IO () 
insertScore g | time (player2 g) == 0 =   getScore SinglePlayer >>= B.writeFile "SingleBoard.json" . A.encode . (entry :)
              | otherwise = getScore Coop >>= B.writeFile "CoopBoard.json" . A.encode  .(entry :)
  where entry =  ScoreEntry (playerName g) newScore
        newScore = round ( time (player2 g) + time (player1 g))

 --gives the list is score entries from the selected gamemode out of the corresponding json file
getScore :: GameMode -> IO [ScoreEntry]
getScore m = list <$> mlist m
  where mlist SinglePlayer = A.decodeFileStrict "SingleBoard.json"
        mlist Coop = A.decodeFileStrict "CoopBoard.json"
        list (Just a) = a
        list Nothing = []

 -- takes both highscore lists and write them both onto the screen next to each other
showLeaderboard :: GameMode -> Float -> IO Picture
showLeaderboard g f =  drawBoard 300 <$> board (getTop <$> getScore g)
  where getTop =take 6 .sortBy (\(ScoreEntry _ a) (ScoreEntry _ b) -> compare b a) --after sorting only the top 6 is returned so it'll fit on the screen
        board = fmap (foldl addEntry [title g] ) 
        addEntry list (ScoreEntry n s)  = list ++ ["Name:" ++ n ++ " Score:" ++ show s  ]
        drawBoard i [] = translate f i $ scale 0.45 0.45 $ color white (text "~~~~~~~~~~~~~~~~~~")
        drawBoard i (p:ps) = pictures[translate f i (  scale 0.45 0.45 $ color white (text p)) , drawBoard (i-70) ps]
        title SinglePlayer = "SinglePlayer"
        title Coop = "Coop"

