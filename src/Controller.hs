module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState -- flow between different  states (eventKey will probably be replaced with mouseinput)
inputKey (EventKey (Char 'n') _ _ _) gstate@(GameState Main _ _ _ _) = initialState {currentState = Choose}  -- new game
inputKey (EventKey (Char 'c') _ _ _) gstate@(GameState Main _ _ _ _) = gstate {currentState = Playing}      -- continue game
inputKey (EventKey (Char '1') _ _ _) gstate@(GameState Choose _ _ _ _) = gstate {currentState = Playing, player2 = (player2 gstate){lives = 0}} -- singeplayer
inputKey (EventKey (Char '2') _ _ _) gstate@(GameState Choose _ _ _ _) = gstate {currentState = Playing } -- coop
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Playing _ _ _ _) = gstate {currentState = Pause} --pause game
inputKey (EventKey (Char 'c') _ _ _) gstate@(GameState Pause _ _ _ _) = gstate {currentState = Playing} -- contine game
inputKey (EventKey (Char 'm') _ _ _) gstate@(GameState Pause _ _ _ _) = gstate {currentState = Main}  -- back to main menu (without losing progress)
inputKey (EventKey (Char 'l') _ _ _) gstate@(GameState Main _ _ _ _) = gstate {currentState = Leaderboard} -- view leaderboard
inputKey (EventKey (Char 'm') _ _ _) gstate@(GameState Leaderboard _ _ _ _) = gstate {currentState = Main} -- back to main menu 
inputKey _ gstate = gstate

