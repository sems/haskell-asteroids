-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
  currentState :: State,
  player1 :: Player,
  player2 :: Player,
  asteroids :: [Asteroid],
  bullets :: [Bullet] 
}

Data Player = Player{
  lives :: Int,
  position :: (Float, Float),
  direction :: (Float, Float),
  time :: Float 
}

Data Bullet = Bullet {
  position :: (Float, Float ),
  direction :: (Float, Float) 
}

Data Asteroid = Asteroid {
  position :: (Float, Float),
  direction :: (Float, Float),
  size :: Int,
  speed :: Int 
}

Data ScoreEntry = ScoreEntry {
  name :: String,
  score :: Int,
  mode :: GameMode 
}

Data State = Main
  | Playing
  | GameOver
  | Pause
  | Leaderboard
  | Choose


Data GameMode = Co-Op | SinglePlayer

initialState :: GameState
initialState = GameState ShowNothing 0