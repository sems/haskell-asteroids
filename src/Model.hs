{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import System.Random
import Data.Set
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Interact (Key)
import GHC.Generics

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
  bullets      :: [Bullet],
  keys         :: S.Set Key,
  playerName   :: String
}

data Player = Player {
  lives      :: Lives, 
  playerPos  :: Position,
  playerDir  :: Direction,
  time       :: Float 
}
  deriving Eq

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
  score :: Score 
}
  deriving Generic

data State = Main
  | Playing
  | GameOver
  | Pause
  | Leaderboard
  | Choose
  | GetName

data GameMode = Coop | SinglePlayer
  deriving (Show, Eq)

initialAsteroid :: Asteroid
initialAsteroid = Asteroid (0,0) (0,0) 1 1

initialState :: GameState
initialState = GameState Main (Player 5 (-100 , 0) (1,0) 0) (Player 5 (100,0) (1,0) 0) [] [] S.empty ""
