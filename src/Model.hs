{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Interface.IO.Interact (Key)

import qualified Data.Set as S
import GHC.Generics ( Generic )

type Direction = (Float, Float)
type Position  = (Float, Float)
type Lives     = Int
type Size      = Int
type Speed     = Float 
type Score     = Int
type Time      = Float

data GameState = GameState {
  currentState :: State,
  player1      :: Player,
  player2      :: Player,
  asteroids    :: [Asteroid],
  bullets      :: [Bullet],
  keys         :: S.Set Key,
  playerName   :: String,
  collision    :: [(Position, Time)]
}

data Player = Player {
  lives      :: Lives, 
  playerPos  :: Position,
  playerDir  :: Direction,
  time       :: Time 
} deriving Eq

data Bullet = Bullet {
  bulletPos  :: Position,
  bulletDir  :: Direction 
}

data Asteroid = Asteroid {
  asteriodPos  :: Position,
  asteriodDir  :: Direction,
  size         :: Size,
  speed        :: Speed 
}

data ScoreEntry = ScoreEntry {
  name  :: String,
  score :: Score 
} deriving Generic

data State = Main
  | Playing
  | GameOver
  | Pause
  | Leaderboard
  | Choose
  | GetName

data GameMode = Coop | SinglePlayer
  deriving (Show, Eq)

data MoveDirection = UpDir | DownDir | LeftDir | RightDir
  deriving Eq

data Button = Button {
  bWidth :: Float ,
  bPos :: Position,
  bText :: String
}

-- all present mouse buttons 
bContinueM :: Button
bContinueM = Button 530 (-265,0) "Continue" -- button to continue an already started game from the mainmenu

bContinueP = Button 530 (-265, 0) "Continue" -- continue from pause menu

bNewGame = Button 750 (-375,-150) "New Game" -- start a new game

bSingle = Button 800 (-400, -200) "Single Player" -- choose single player

bCoop = Button 300 (-150, 0) "Coop" -- choose coop

bLeaderM = Button 750 (-375, -300) "Leaderboard" -- go to leaderboard from main menu

bLeaderP = Button 750 (-375,-300) "Leaderboard" -- go to leaderboard from pause

bLeaderG = Button 750 (-375,-300) "Leaderboard" -- g to leaderboard from gameover

bMainP = Button 700 (-350,-150) "Main Menu" -- go to main menu from pause

bMainG = Button 700 (-350,-150) "Main Menu" -- go to mainmenu from gameover

bMainL = Button 700 (-350,-350) "Main Menu" -- go to main menu from leaderboard

initialState :: GameState
initialState = GameState Main (Player 5 (-100 , 0) (1,0) 0) (Player 5 (100,0) (1,0) 0) [] [] S.empty "Nameless" []
