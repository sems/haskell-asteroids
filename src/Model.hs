-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

type Direction = (Float, Float)
type Position = (Float, Float)

data GameState = GameState {
  currentState :: State,
  player1      :: Player,
  player2      :: Player,
  asteroids    :: [Asteroid],
  bullets      :: [Bullet] 
}

data Player = Player {
  lives     :: Int, 
  position  :: Position,
  direction :: Direction,
  time      :: Float 
}

data Bullet = Bullet {
  position  :: Position,
  direction :: Direction 
}

data Asteroid = Asteroid {
  position  :: Position,
  direction :: Direction,
  size      :: Int,
  speed     :: Int 
}

data ScoreEntry = ScoreEntry {
  name  :: String,
  score :: Int,
  mode  :: GameMode 
}

data State = Main
  | Playing
  | GameOver
  | Pause
  | Leaderboard
  | Choose

data GameMode = Coop | SinglePlayer

initialState :: GameState
initialState = GameState ShowNothing 0