-- | This module contains the data types
--   which represent the state of the game
module Model where
import System.Random

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

type Direction = (Float, Float)
type Position  = (Float, Float)
type Lives     = Int
type Size      = Int
type Speed     = Float 
type Score     = Int

data GameState = GameState {
  currentState :: State,
  player1      :: Player,
  player2      :: Player,
  asteroids    :: [Asteroid],
  bullets      :: [Bullet] 
}

data Player = Player {
  lives      :: Lives, 
  playerPos  :: Position,
  playerDir  :: Direction,
  time       :: Float 
}

data Bullet = Bullet {
  bulletPos  :: Position,
  bulletDir  :: Direction 
}

{-
  Size are in the range of [1 .. 4]
  Speed is in the range of [0.5 .. 2.0]
-}
data Asteroid = Asteroid {
  asteriodPos  :: Position,
  asteriodDir  :: Direction,
  size         :: Size,
  speed        :: Speed 
}

data ScoreEntry = ScoreEntry {
  name  :: String,
  score :: Score,
  mode  :: GameMode 
}

data State = Main
  | Playing
  | GameOver
  | Pause
  | Leaderboard
  | Choose

data GameMode = Coop | SinglePlayer

initialPlayer :: Player
initialPlayer = Player 5 (0,0) (0,0) 0.0

initialAsteroid :: Asteroid
initialAsteroid = Asteroid (0,0) (0,0) 1 1

initialState :: GameState
initialState = GameState Main initialPlayer initialPlayer [] []
